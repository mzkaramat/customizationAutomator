#Database Configuration Part
database_ip="10.128.5.34"
database_user="flexcube3"
database_password="flex"
database_port="1521"
database_sid="fcubs"

rm -r temp

mkdir temp
mkdir temp/to_deploy
unzip RAD_3.ZIP -d ./temp

cd temp/INC
ls -1 > ../INC.txt
cd ..
sed -i -e 's/^/@"D:\\Flexcube\\Deploy Automator\\Sandbox\\temp\\INC\\/' INC.txt
sed -i 's/$/"/' INC.txt

cd SPC
ls -1 >> ../SPC.txt
cd ..
sed -i -e 's/^/@"D:\\Flexcube\\Deploy Automator\\Sandbox\\temp\\SPC\\/' SPC.txt
sed -i 's/$/"/' SPC.txt


cd SQL
ls -1 >> ../SQL.txt
cd ..
sed -i -e 's/^/@"D:\\Flexcube\\Deploy Automator\\Sandbox\\temp\\SQL\\/' SQL.txt
sed -i 's/$/"/' SQL.txt

cat INC.txt SPC.txt SQL.txt > to_deploy.txt

db_connec_string="$database_user/$database_password@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(Host=$database_ip)(Port=$database_port))(CONNECT_DATA=(SID=$database_sid)))"
echo $db_connec_string

echo exit | sqlplus $db_connec_string  @"to_deploy.txt" 

scp JS/CIDTKF_SYS.js oracle@10.128.5.34:/u01/middleware/user_projects/domains/base_domain/servers/mngdsrvr1/tmp/_WL_user/FCUBSApp_12.0.3.0_12.0.3.8.3/a6v1be/war/Script/JS

scp UIXML/ENG/CIDTKF.xml oracle@10.128.5.34:/u01/middleware/user_projects/domains/base_domain/servers/mngdsrvr1/tmp/_WL_user/FCUBSApp_12.0.3.0_12.0.3.8.3/a6v1be/war/UIXML/ENG

ssh oracle@10.128.5.34 'rm /u01/middleware/user_projects/domains/base_domain/servers/mngdsrvr1/tmp/_WL_user/FCUBSApp_12.0.3.0_12.0.3.8.3/a6v1be/war/Script/JS/SYS/CIDTKF_SYS.js'
#sshpass -p 'oracle123' 
