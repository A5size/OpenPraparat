src="../build"
dst="../package"
trg="praparat_gui.exe"

if [ ! -f ${src}"/"${trg} ]; then
    echo "Error: ${src}"/"${trg} not found."
    exit 1
fi

if [ -d ${dst} ]; then
    echo "Error: ${dst} found."
    echo "Remove ${dst} and try again."
    exit 1
fi

cp -r ${src} ${dst}
cp ../ThirdPartyNotices.txt ${dst}/
cp ../LICENSE.md ${dst}/

for dll in `ldd ${src}/${trg} | grep -v /c/ | awk '{print $3}'`;
do
    echo ${dll}
    cp ${dll} ${dst}/
done
