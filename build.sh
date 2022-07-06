#!/bin/sh
exec discount-theme \
	-c +autolink,+idanchor \
	-t README.theme -f \
	-o index.html README.md
