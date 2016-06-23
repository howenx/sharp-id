#!/usr/bin/env bash
./activator-1.3.10-minimal/bin/activator clean stage
echo '123123'|sudo -S kill -9 `sudo lsof -t -i:9004`
echo '123123'|sudo -S rm -rf target/universal/stage/RUNNING_PID
target/universal/stage/bin/style-id -Dhttp.port=9004 -Dconfig.file=target/universal/stage/conf/application.conf -Dlogger.resource=logback.xml
echo " - Finished Deploy"
