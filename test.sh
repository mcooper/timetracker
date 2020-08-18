cpan -i X11::IdleTime # or any otherway to install X11::IdleTime module
sleep 2
perl -MX11::IdleTime -e 'print GetIdleTime(), $/;'
