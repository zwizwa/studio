# FIXME!
CARGO=~/.cargo/bin/cargo

# FIXME: How to get cargo to display full paths in error messages?
# This is here as a workaround for emacs compile-mode.
if [ ! -z "REDO_VERBOSE_ENTER" ]; then 
    echo "redo: Entering directory '$(readlink -f .)'" >&2
fi

RS=$(find -name '*.rs')
redo-ifchange $RS Cargo.toml
$CARGO check || exit 1
$CARGO build || exit 1
ln target/debug/studio_rs $3
