#!/bin/sh
exec discount-theme \
	-c +autolink,+idanchor \
	-t manual.theme -f \
	-o docs/index.html manual.md
