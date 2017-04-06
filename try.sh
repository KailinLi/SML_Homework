TIME=`date "+%m/%d/%Y"`
git add *
git commit -m "$TIME"
git push origin master
echo "$TIME"