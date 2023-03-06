src="../build"
dst="../packages"
trg="praparat_gui.exe"

pnm="OpenPraparat-v0.0.1-beta"

if [ ! -f ${src}"/"${trg} ]; then
    echo "Error: ${src}"/"${trg} not found."
    exit 1
fi

if [ -d ${dst}/${pnm} ]; then
    echo "Error: ${dst}"/"${pnm} already exists."
    exit 1
fi

if [ ! -d ${dst} ]; then
    mkdir ${dst}
fi

cp -r ${src} ${dst}/${pnm}
cp ../ThirdPartyNotices.txt ${dst}/${pnm}/
cp ../LICENSE.md ${dst}/${pnm}/
tar -zxvf ../examples.tar.gz -C ${dst}/${pnm}/

for dll in `ldd ${src}/${trg} | grep -v /c/ | awk '{print $3}'`;
do
    echo ${dll}
    cp ${dll} ${dst}/${pnm}/
done

cwd=`pwd`
cd ${dst}
tar -zcvf ${pnm}.tar.gz ${pnm}
zip -r    ${pnm}.zip    ${pnm}
cd ${cwd}
