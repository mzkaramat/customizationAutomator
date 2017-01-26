/***************************************************************************************************************************
**  This source is part of the FLEXCUBE Software Product. 
**  Copyright (c) 2008 ,2017, Oracle and/or its affiliates.
**  All rights reserved.
**  
**  No part of this work may be reproduced, stored in a retrieval system, 
**  adopted or transmitted in any form or by any means, electronic, mechanical, photographic, 
**  graphic, optic recording or otherwise, translated in any language or computer language, 
**  without the prior written permission of Oracle and/or its affiliates.
**  
**  Oracle Financial Services Software Limited.
**  Oracle Park, Off Western Express Highway,
**  Goregaon (East),
**  Mumbai - 400 063,
**  India.
**  
**  Written by         : 
**  Date of creation   : 
**  File Name          : CIDTKF_SYS.js
**  Purpose            : 
**  Called From        : 
**  
**  CHANGE LOG
**  Last Modified By   : 
**  Last modified on   : 
**  Full Version       : 
**  Reason             : 
****************************************************************************************************************************/

//----------------------------------------------------------------------------------------------------------------------
//***** FCJ XML FOR THE SCREEN *****
//----------------------------------------------------------------------------------------------------------------------
var msgxml=""; 
msgxml += '    <FLD>'; 
msgxml += '      <FN PARENT="" RELATION_TYPE="1" TYPE="BLK_ACC">ACC~BRN</FN>'; 
msgxml += '      <FN PARENT="BLK_ACC" RELATION_TYPE="N" TYPE="BLK_TKF">YEARS~EFF_DT~BASIS_AMT~DEP_RATE~INSR_RATE~PREM_AMT~POL_NUM~EXP_DT</FN>'; 
msgxml += '    </FLD>'; 

var strScreenName = "CVS_MAIN";
var txnBranchFld = "" ;
//----------------------------------------------------------------------------------------------------------------------
//***** CODE FOR DATABINDING *****
//----------------------------------------------------------------------------------------------------------------------
var relationArray = new Array(); 			// {Table Name} is the array index, {Parent Table Name}~{Relation} is the array value 
relationArray['BLK_ACC'] = ""; 
relationArray['BLK_TKF'] = "BLK_ACC~N"; 

var dataSrcLocationArray = new Array(); 	// Array of all Data Sources used in the screen 
dataSrcLocationArray[0] = "BLK_ACC"; 
dataSrcLocationArray[1] = "BLK_TKF"; 
//----------------------------------------------------------------------------------------------------------------------
//***** CODE FOR QUERY MODE *****
//----------------------------------------------------------------------------------------------------------------------
var detailRequired = true ;
var intCurrentQueryResultIndex = 0;
var intCurrentQueryRecordCount = 0;

var queryFields = new Array();    //Values should be set inside CIDTKF.js, in "BlockName__FieldName" format
var pkFields    = new Array();    //Values should be set inside CIDTKF.js, in "BlockName__FieldName" format
queryFields[0] = "BLK_ACC__ACC";
pkFields[0] = "BLK_ACC__ACC";
queryFields[1] = "BLK_ACC__BRN";
pkFields[1] = "BLK_ACC__BRN";
//----------------------------------------------------------------------------------------------------------------------
//***** CODE FOR AMENDABLE/SUBSYSTEM Fields *****
//----------------------------------------------------------------------------------------------------------------------
var modifyAmendArr = new Array(); 
var closeAmendArr = new Array(); 
var reopenAmendArr = new Array(); 
var reverseAmendArr = new Array(); 
var deleteAmendArr = new Array(); 
var rolloverAmendArr = new Array(); 
var confirmAmendArr = new Array(); 
var liquidateAmendArr = new Array(); 
var queryAmendArr = new Array(); 
var authorizeAmendArr = new Array(); 
var subsysArr    = new Array(); 

//***** Fields Amendable while Modification *****
modifyAmendArr[0] = "BLK_TKF__EFF_DTI";
modifyAmendArr[1] = "BLK_TKF__EXP_DTI";
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------

//***** CODE FOR LOVs *****
//----------------------------------------------------------------------------------------------------------------------
var retflds = new Array();
var bndFlds = new Array();
var lovVal = new Array();
var indexFlds = new Array();

var offlineRetflds = new Array();
var offlineBndFlds = new Array();


/***** Lov Infomation for Field BLK_ACC__ACC  *****/
retflds["BLK_ACC__ACC__LOV_ACC"]="BLK_ACC__ACC~";
bndFlds["BLK_ACC__ACC__LOV_ACC"]="BLK_ACC__BRN!VARCHAR2";
indexFlds["BLK_ACC__ACC__LOV_ACC"]="N";


/***** Lov Infomation for Field BLK_ACC__BRN  *****/
retflds["BLK_ACC__BRN__LOV_BRN"]="BLK_ACC__BRN~";
bndFlds["BLK_ACC__BRN__LOV_BRN"]="";
indexFlds["BLK_ACC__BRN__LOV_BRN"]="N";

//----------------------------------------------------------------------------------------------------------------------
//***** SCRIPT FOR TABS *****
//----------------------------------------------------------------------------------------------------------------------
var strHeaderTabId = 'TAB_HEADER';
var strFooterTabId = 'TAB_FOOTER';
var strCurrentTabId = 'TAB_MAIN';
//--------------------------------------------
//----------------------------------------------------------------------------------------------------------------------
//***** SCRIPT FOR MULTIPLE ENTRY BLOCKS *****
//----------------------------------------------------------------------------------------------------------------------
var multipleEntryIDs = new Array();
var multipleEntryArray = new Array();
var multipleEntryCells = new Array();
//--------------------------------------------
multipleEntryIDs[0] = 'BLK_TKF';
//--------------------------------------------
//----------------------------------------------------------------------------------------------------------------------
//***** SCRIPT FOR MULTIPLE ENTRY VIEW SINGLE ENTRY BLOCKS *****
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
//***** SCRIPT FOR ATTACHED CALLFORMS *****
 //----------------------------------------------------------------------------------------------------------------------

 var CallFormArray= new Array();

 var CallFormRelat=new Array();

 var CallRelatType= new Array();


 var ArrFuncOrigin=new Array();
 var ArrPrntFunc=new Array();
 var ArrPrntOrigin=new Array();
 var ArrRoutingType=new Array();


 // Code for Loading Cluster/Custom js File Starts
 var ArrClusterModified=new Array();
 var ArrCustomModified=new Array();
 // Code for Loading Cluster/Custom js File ends

ArrFuncOrigin["CIDTKF"]="CUSTOM";
ArrPrntFunc["CIDTKF"]="";
ArrPrntOrigin["CIDTKF"]="";
ArrRoutingType["CIDTKF"]="X";


 // Code for Loading Cluster/Custom js File Starts
ArrClusterModified["CIDTKF"]="N";
ArrCustomModified["CIDTKF"]="Y";

 // Code for Loading Cluster/Custom js File ends

//***** CODE FOR SCREEN ARGS *****
//----------------------------------------------------------------------------------------------------------------------
var scrArgName = new Array(); 
var scrArgSource = new Array(); 
var scrArgDest = new Array(); 
var scrArgVals = new Array(); 
//***** CODE FOR SUB-SYSTEM DEPENDENT  FIELDS   *****
//----------------------------------------------------------------------------------------------------------------------
var dpndntOnFlds = new Array(); 
var dpndntOnSrvs = new Array(); 
//***** CODE FOR TAB DEPENDENT  FIELDS   *****
//----------------------------------------------------------------------------------------------------------------------
//***** CODE FOR CALLFORM TABS *****
//----------------------------------------------------------------------------------------------------------------------
var callformTabArray = new Array(); 
//***** CODE FOR ACTION STAGE DETAILS *****
//----------------------------------------------------------------------------------------------------------------------
var actStageArry = new Array(); 
actStageArry['QUERY'] = "2";
actStageArry['NEW'] = "2";
actStageArry['MODIFY'] = "2";
actStageArry['AUTHORIZE'] = "1";
actStageArry['DELETE'] = "1";
actStageArry['CLOSE'] = "1";
actStageArry['REOPEN'] = "1";
actStageArry['REVERSE'] = "1";
actStageArry['ROLLOVER'] = "1";
actStageArry['CONFIRM'] = "1";
actStageArry['LIQUIDATE'] = "1";
actStageArry['SUMMARYQUERY'] = "2";