#!/bin/bash
rm -f "Site Folder/.DS_Store"
cd "Site Folder" && \
	scp -r * linode-prod:/var/www/scottraffic/support/ &&
	scp -r * linode-dev:/var/www/scottraffic/support/


