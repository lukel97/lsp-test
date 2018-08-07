#!/bin/sh
case `hie --version` in
	*8.4.3*) wget https://877-129435383-gh.circle-artifacts.com/0/bin/hie;;
	*8.2.2*) wget https://878-129435383-gh.circle-artifacts.com/0/bin/hie;;
esac
