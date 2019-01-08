
make

for file in test_add/good/basic/*.lat
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


for file in test_add/bad/semantic/*.lat
do
  if [ ! -d "$file" ]; then
      echo testing $file
      ./latc_llvm $file > /dev/null
  fi
done


for file in test_add/bad/runtime/*.lat
do
  if [ ! -d "$file" ]; then
      echo testing $file
      ./latc_llvm $file > /dev/null
  fi
done


for file in test_add/bad/infinite_loop/*.lat
do
  if [ ! -d "$file" ]; then
      echo testing $file
      ./latc_llvm $file > /dev/null
  fi
done

for file in test_add/grd5/*.lat
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

