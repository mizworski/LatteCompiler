
make

for file in test/good/*.lat
do
  if [ ! -d "$file" ]; then
      filename="${file%.*}"
      echo testing $file
      ./latc_llvm $file > /dev/null
      if [ -f ${filename}.input ]; then
          lli ${filename}.bc < ${filename}.input > ${filename}_my.output
      else
          lli ${filename}.bc > ${filename}_my.output
      fi
      diff ${filename}_my.output ${filename}.output
  fi
done


for file in test/bad/*.lat
do
  if [ ! -d "$file" ]; then
      echo testing $file
      ./latc_llvm $file > /dev/null
  fi
done