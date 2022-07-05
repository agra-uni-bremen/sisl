#!/bin/sh
exec discount-theme \
	-c +autolink,+idanchor \
	-t README.theme -f \
	-o ./www/index.html README.md
