SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd SCRIPT_DIR

# Clone repository.
mkdir build
cd build
git clone https://github.com/Marchen/cv.models.git
cd cv.models

# Remove extra files.
rm install.r
rm include.r
rm .travis.yml
rm build.bat

# Build package.
R -e 'devtools::document()'
R -e 'devtools::build(path = "..")'

cd ../..
