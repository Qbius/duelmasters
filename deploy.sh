cd $1
bin/dmdbserver stop
tar -zxf tar/dmdbserver-0.1.0.tar.gz
sed -i -e "s/cookiename/$1/g" releases/0.1.0/vm.args
sed -i -e "s/portnumber/$2/g" releases/0.1.0/sys.config
bin/dmdbserver start