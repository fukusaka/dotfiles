# CVS Repository

case "$1" in
    home)
	CVSROOT=":ext:${USER}@cvshost:/data/cvs"
	;;
    *)
esac

export CVSROOT
