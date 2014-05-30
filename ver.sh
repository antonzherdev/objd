./build.sh

rm -rf ./dist/ObjD
rm -rf ./dist/ObjD.zip
mkdir ./dist/ObjD
cp -r ./dist/build/ObjD/ObjD ./dist/ObjD/


cp -r ./ObjDLib/Java ./dist/ObjD/
cp -r ./ObjDLib/ObjC/ObjDLib/ObjDLib/Sources ./dist/ObjD/Java/ObjDLib
mv ./dist/ObjD/Java/ObjDLib/Sources ./dist/ObjD/Java/ObjDLib/src
rm -rf ./dist/ObjD/Java/out

cp -r ./ObjDLib/ObjC ./dist/ObjD/

zip -r -q ./dist/ObjD.zip ./dist/ObjD
