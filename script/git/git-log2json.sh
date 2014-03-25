#!/usr/bin/env bash
 
# Use this one-liner to produce a JSON literal from the Git log:

cd /Users/gust/workspace/gust ; git fetch acceptance ; git log acceptance/master -300 \
    --pretty=format:'{%n  "commit": "%H",%n  "author": "%an <%ae>",%n  "date": "%ad",%n  "message": "%f"%n},' \
    $@ | \
    perl -pe 'BEGIN{print "["}; END{print "]\n"}' | \
    perl -pe 's/},]/}]/'
