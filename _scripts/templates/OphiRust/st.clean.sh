# Generated from trgen <version>
rm -rf Cargo.lock
rm -rf target
find src/gen -mindepth 1 ! -name 'mod.rs' -exec rm -rf '{}' ';'
