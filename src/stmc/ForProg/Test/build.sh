echo "clean"
rm -rf build
rm -rf dist
mkdir build
mkdir dist
echo "working directory = $(pwd)"

for f in *.f; do
  echo "start make $f"
  MODULE=${f//.f/} make -f makefile
  echo "done make $f"

  echo "move object"
  mv ${f//.f/.o} build
  echo "object is located in build"

  echo "move binary"
  mv ${f//.f/.out} dist
  echo "binary is located in dist"
done

