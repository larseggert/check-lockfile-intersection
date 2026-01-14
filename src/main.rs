use attohttpc::get;
use cargo_lock::{Lockfile, Name, Package};
use petgraph::{algo::toposort, graph::DiGraph};
use std::{
    collections::{BTreeMap, BTreeSet},
    str::FromStr,
};
use url::Url;

#[derive(Default)]
struct Spec {
    src: String,
    pkg_name: Option<String>,
    pkg_hash: Option<String>,
    exclude_pkgs: BTreeSet<String>,
}

#[derive(Default)]
struct Args {
    spec_a: Spec,
    spec_b: Spec,
    verbose: bool,
}

fn comma_separated_list(s: &Option<String>) -> Vec<String> {
    if let Some(s) = s {
        s.split(',').map(|x| x.to_string()).collect()
    } else {
        Vec::new()
    }
}

impl Args {
    fn parse() -> Result<Self, String> {
        let mut args = Args::default();
        let flags = xflags::parse_or_exit! {
            /// Limit first lockfile to package tree rooted at hash (git commit or crate checksum)
            optional --pkg-hash-a hash_a: String
            /// Limit second lockfile to package tree rooted at hash (git commit or crate checksum)
            optional --pkg-hash-b hash_b: String
            /// Limit first lockfile to package tree rooted at package name
            optional --pkg-name-a name_a: String
            /// Limit second lockfile to package tree rooted at package name
            optional --pkg-name-b name_b: String
            /// Comma-separated list of packages to exclude from first lockfile
            optional --exclude-pkg-a exclude_a: String
            /// Comma-separated list of packages to exclude from second lockfile
            optional --exclude-pkg-b exclude_b: String
            /// First lockfile (URL or path)
            required lockfile_a: String
            /// Second lockfile (URL or path)
            required lockfile_b: String
            /// Print more details while running
            optional --verbose
        };
        args.spec_a.pkg_hash = flags.pkg_hash_a;
        args.spec_b.pkg_hash = flags.pkg_hash_b;
        args.spec_a.pkg_name = flags.pkg_name_a;
        args.spec_b.pkg_name = flags.pkg_name_b;
        args.spec_a.exclude_pkgs = comma_separated_list(&flags.exclude_pkg_a)
            .into_iter()
            .collect();
        args.spec_b.exclude_pkgs = comma_separated_list(&flags.exclude_pkg_b)
            .into_iter()
            .collect();
        args.spec_a.src = flags.lockfile_a;
        args.spec_b.src = flags.lockfile_b;
        args.verbose = flags.verbose;
        Ok(args)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Phase {
    NameIntersection,
    NameAndVersionIntersection,
}

struct State {
    spec: Spec,
    lockfile: Lockfile,
    packages: BTreeMap<Name, (Package, Vec<Package>)>,
    phase: Phase,
    verbose: bool,
}

fn load_lockfile(src: &str) -> Result<Lockfile, String> {
    let url = match Url::parse(src) {
        Ok(url) => url,
        Err(url::ParseError::RelativeUrlWithoutBase) => {
            // Possibly we were just given a plain file path, try that.
            return Lockfile::load(src).map_err(|e| e.to_string());
        }
        Err(e) => return Err(e.to_string()),
    };
    if url.scheme() == "file" {
        let path = url
            .to_file_path()
            .map_err(|_| "file URL error".to_string())?;
        Lockfile::load(path).map_err(|e| e.to_string())
    } else if url.scheme() == "http" || url.scheme() == "https" {
        let response = get(url.as_str()).send().map_err(|e| e.to_string())?;
        if !response.is_success() {
            return Err(format!(
                "Failed to fetch lockfile: {}",
                response.text().unwrap_or_default()
            ));
        }
        let text = response.text().map_err(|e| e.to_string())?;
        Lockfile::from_str(&text).map_err(|e| e.to_string())
    } else {
        Err(format!("Unsupported URL scheme: {}", url.scheme()))
    }
}

fn package_matches_hash(pkg: &cargo_lock::Package, hash: &str) -> bool {
    // Try comparing hash to hashes in either the package checksum or the source
    // precise field
    if let Some(cksum) = &pkg.checksum {
        if cksum.to_string() == hash {
            return true;
        }
    }
    if let Some(src) = &pkg.source {
        if let Some(precise) = src.precise() {
            if precise == hash {
                return true;
            }
        }
    }
    false
}

impl State {
    fn new(spec: Spec, verbose: bool) -> Result<Self, String> {
        let lockfile = load_lockfile(&spec.src)?;
        Ok(State {
            spec,
            lockfile,
            phase: Phase::NameIntersection,
            packages: BTreeMap::new(),
            verbose,
        })
    }

    fn try_insert_package(&mut self, package: &Package, path: &[Package]) -> Result<bool, String> {
        if let Some(existing) = self.packages.get_mut(&package.name) {
            if self.phase == Phase::NameAndVersionIntersection
                && existing.0.version != package.version
            {
                existing.0 = package.clone();
                existing.1 = path.to_vec();
            }
            Ok(false)
        } else {
            if self.verbose {
                println!(
                    "found {} {} {}",
                    self.spec.src, package.name, package.version
                );
            }
            self.packages
                .insert(package.name.clone(), (package.clone(), path.to_vec()));
            Ok(true)
        }
    }

    fn add_all_dependencies_recursive(
        &mut self,
        package: &Package,
        path: &mut Vec<Package>,
    ) -> Result<(), String> {
        for dep in package.dependencies.iter() {
            if self.spec.exclude_pkgs.contains(dep.name.as_str()) {
                continue;
            }
            let dep_pkg = self
                .lockfile
                .packages
                .iter().find(|&p| dep.matches(p)).cloned();
            if let Some(dep_pkg) = dep_pkg {
                path.push(dep_pkg.clone());
                if self.try_insert_package(&dep_pkg, path)? {
                    self.add_all_dependencies_recursive(&dep_pkg, path)?;
                }
                path.pop();
            }
        }
        Ok(())
    }

    fn add_packages_in_dependency_tree(&mut self) -> Result<(), String> {
        let all_packages = self.lockfile.packages.to_vec();
        for package in all_packages.iter() {
            if self.spec.exclude_pkgs.contains(package.name.as_str()) {
                continue;
            }
            if let Some(name) = &self.spec.pkg_name {
                if package.name.as_str() != *name {
                    continue;
                }
            }
            if let Some(hash) = &self.spec.pkg_hash {
                if !package_matches_hash(package, hash) {
                    continue;
                }
            }

            let mut path = vec![package.clone()];
            self.packages
                .insert(package.name.clone(), (package.clone(), path.clone()));
            self.add_all_dependencies_recursive(package, &mut path)?;
            return Ok(());
        }
        Err(format!(
            "No package named {:?} with hash {:?} found in lockfile {}",
            self.spec.pkg_name, self.spec.pkg_hash, self.spec.src
        ))
    }

    fn add_all_packages_in_lockfile(&mut self) -> Result<(), String> {
        let all_packages = self
            .lockfile
            .packages
            .iter().filter(|&x| !self.spec.exclude_pkgs.contains(x.name.as_str())).cloned()
            .collect::<Vec<_>>();
        let mut path = Vec::new();
        for package in all_packages {
            path.push(package.clone());
            self.try_insert_package(&package, &path)?;
            path.pop();
        }
        Ok(())
    }

    fn add_packages(&mut self) -> Result<(), String> {
        if self.spec.pkg_name.is_some() || self.spec.pkg_hash.is_some() {
            self.add_packages_in_dependency_tree()
        } else {
            self.add_all_packages_in_lockfile()
        }
    }
}

struct Program {
    state_a: State,
    state_b: State,
}

impl Program {
    fn new() -> Result<Self, String> {
        let args = Args::parse()?;
        let state_a = State::new(args.spec_a, args.verbose)?;
        let state_b = State::new(args.spec_b, args.verbose)?;
        Ok(Program { state_a, state_b })
    }

    fn add_packages_and_calculate_intesection(&mut self) -> Result<BTreeSet<Name>, String> {
        self.state_a.add_packages()?;
        self.state_b.add_packages()?;
        let package_names_a = self.state_a.packages.keys().collect::<BTreeSet<_>>();
        let package_names_b = self.state_b.packages.keys().collect::<BTreeSet<_>>();
        let intersection = package_names_a
            .intersection(&package_names_b)
            .map(|x| (*x).clone())
            .collect::<BTreeSet<Name>>();
        println!("{} packages in lockfile A", package_names_a.len());
        println!("{} packages in lockfile B", package_names_b.len());
        println!("{} packages in common", intersection.len());
        Ok(intersection)
    }

    fn run(&mut self) -> Result<(), String> {
        let first_pass_intersection = self.add_packages_and_calculate_intesection()?;
        println!("excluding packages outside intersection and recalculating");
        let mut excluded_a = 0;
        let mut excluded_b = 0;
        for pkg in self.state_a.packages.keys().cloned().collect::<Vec<_>>() {
            if !first_pass_intersection.contains(&pkg) {
                excluded_a += 1;
                self.state_a
                    .spec
                    .exclude_pkgs
                    .insert(pkg.as_str().to_string());
            }
        }
        for pkg in self.state_b.packages.keys().cloned().collect::<Vec<_>>() {
            if !first_pass_intersection.contains(&pkg) {
                excluded_b += 1;
                self.state_b
                    .spec
                    .exclude_pkgs
                    .insert(pkg.as_str().to_string());
            }
        }
        println!("excluded {} more packages from lockfile A", excluded_a);
        println!("excluded {} more packages from lockfile B", excluded_b);
        self.state_a.phase = Phase::NameAndVersionIntersection;
        self.state_b.phase = Phase::NameAndVersionIntersection;
        self.state_a.packages.clear();
        self.state_b.packages.clear();
        let intersection = self.add_packages_and_calculate_intesection()?;

        // Collect packages with different versions
        let mut different: BTreeMap<Name, &Package> = BTreeMap::new();
        for name in intersection.iter() {
            let (pkg_a, _) = self.state_a.packages.get(name).unwrap();
            let (pkg_b, _) = self.state_b.packages.get(name).unwrap();
            // Skip workspace crates (no source means it's a local/workspace package)
            if pkg_b.source.is_none() {
                continue;
            }
            if pkg_a.version == pkg_b.version {
                if self.state_a.verbose {
                    println!("SAME {} {}", name, pkg_a.version);
                }
            } else {
                different.insert(name.clone(), pkg_a);
            }
        }

        if different.is_empty() {
            println!("All packages have the same versions");
            return Ok(());
        }

        // Find packages only in lockfile B that depend on packages we're updating.
        // These may block updates due to version constraints.
        let mut blocking_deps: BTreeMap<Name, Vec<&Package>> = BTreeMap::new();
        for pkg in &self.state_b.lockfile.packages {
            // Skip packages that are in both lockfiles
            if intersection.contains(&pkg.name) {
                continue;
            }
            // Skip workspace crates
            if pkg.source.is_none() {
                continue;
            }
            // Check if this B-only package depends on any package we're updating
            for dep in &pkg.dependencies {
                let dep_name = Name::from_str(dep.name.as_str()).unwrap();
                if different.contains_key(&dep_name) {
                    blocking_deps.entry(dep_name).or_default().push(pkg);
                }
            }
        }

        // Build dependency graph and topologically sort.
        // Edges go from dependent to dependency; toposort returns dependents first.
        let mut graph = DiGraph::<Name, ()>::new();
        let mut node_indices: BTreeMap<Name, _> = BTreeMap::new();

        for name in different.keys() {
            node_indices.insert(name.clone(), graph.add_node(name.clone()));
        }

        for (name, pkg) in &different {
            for dep in &pkg.dependencies {
                let dep_name = Name::from_str(dep.name.as_str()).unwrap();
                if let Some(&dep_idx) = node_indices.get(&dep_name) {
                    graph.add_edge(node_indices[name], dep_idx, ());
                }
            }
        }

        let sorted = toposort(&graph, None).expect("dependency cycle detected");

        // Output cargo update commands in sorted order (dependents first)
        for idx in &sorted {
            let name = &graph[*idx];
            let pkg_a = different.get(name).unwrap();
            // Warn about B-only packages that may block this update
            if let Some(blockers) = blocking_deps.get(name) {
                for blocker in blockers {
                    println!(
                        "# {} {} (only in B) depends on {}; may need: cargo update -p {}",
                        blocker.name, blocker.version, name, blocker.name
                    );
                }
            }
            println!("cargo update -p {} --precise {}", name, pkg_a.version);
        }

        Err("Some packages have different versions".to_string())
    }
}

fn main() -> Result<(), String> {
    let mut program = Program::new()?;
    program.run()
}
