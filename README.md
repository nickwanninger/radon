# radon


## Building

First, you have to have the correct llvm version installed. radon requires llvm-7.

macOS (homebrew):
```bash
brew install llvm-hs/llvm/llvm-7
```


debian/Ubuntu (apt):
```bash
apt-get install llvm-7-dev
```


arch/manjaro (pacman):
```bash
sudo pacman -Sy llvm7
```


Then, simply run the following commands in the root of the project:
```bash
stack setup
stack build
```
