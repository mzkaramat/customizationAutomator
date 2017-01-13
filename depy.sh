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

echo exit | sqlplus "flexcube3/flex@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(Host=10.128.5.34)(Port=1521))(CONNECT_DATA=(SID=fcubs)))"  @"to_deploy.txt" 

scp JS/CIDTKF_SYS.js oracle@10.128.5.34:/u01/middleware/user_projects/domains/base_domain/servers/mngdsrvr1/tmp/_WL_user/FCUBSApp_12.0.3.0_12.0.3.8.3/a6v1be/war/Script/JS

scp UIXML/ENG/CIDTKF.xml oracle@10.128.5.34:/u01/middleware/user_projects/domains/base_domain/servers/mngdsrvr1/tmp/_WL_user/FCUBSApp_12.0.3.0_12.0.3.8.3/a6v1be/war/Script/

ssh oracle@10.128.5.34 'rm /u01/middleware/user_projects/domains/base_domain/servers/mngdsrvr1/tmp/_WL_user/FCUBSApp_12.0.3.0_12.0.3.8.3/a6v1be/war/Script/JS/SYS/CIDTKF_SYS.js'
#sshpass -p 'oracle123' 
