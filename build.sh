#!/bin/sh
exec discount-theme \
	-c +autolink,+idanchor \
	-t manual.theme -f \
	-o index.html manual.md
