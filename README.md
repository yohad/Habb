# Build Instruction
from inside the directory run `nix-build`. It will generate an executable `./result/bin/Habb-exe`.
After a change in `Habb.cabal` you need to run `cabal2nix --shell . > default.nix` to regenerate the build file.
