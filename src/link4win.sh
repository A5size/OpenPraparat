for dll in `ldd ../build/praparat_gui.exe | grep -v /c/ | awk '{print $3}'`;
do
    echo ${dll}
    ln -s ${dll} ../build/
done
