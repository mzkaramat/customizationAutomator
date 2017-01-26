CREATE OR REPLACE PACKAGE BODY cipks_cidtkf_main AS
     /*-----------------------------------------------------------------------------------------------------
     **
     ** File Name  : cipks_cidtkf_main.sql
     **
     ** Module     : Islamic Financing
     ** 
     ** This source is part of the Oracle FLEXCUBE Software Product.
     ** Copyright (R) 2008,2017 , Oracle and/or its affiliates.  All rights reserved
     ** 
     ** 
     ** No part of this work may be reproduced, stored in a retrieval system, adopted 
     ** or transmitted in any form or by any means, electronic, mechanical, 
     ** photographic, graphic, optic recording or otherwise, translated in any 
     ** language or computer language, without the prior written permission of 
     ** Oracle and/or its affiliates. 
     ** 
     ** Oracle Financial Services Software Limited.
     ** Oracle Park, Off Western Express Highway,
     ** Goregaon (East), 
     ** Mumbai - 400 063, India
     ** India
     -------------------------------------------------------------------------------------------------------
     CHANGE HISTORY
     
     SFR Number         :  
     Changed By         :  
     Change Description :  
     
     -------------------------------------------------------------------------------------------------------
     */
     

   g_Ui_Name            VARCHAR2(50) := 'CIDTKF';
   g_cidtkf         cipks_cidtkf_Main.ty_cidtkf;
   g_Req_Key                 VARCHAR2(32767);
   --Skip Handler Variables
   g_Skip_Sys       BOOLEAN := FALSE;
   g_Skip_Custom    BOOLEAN := FALSE;
   PROCEDURE Dbg(p_msg VARCHAR2)  IS
      l_Msg     VARCHAR2(32767);
   BEGIN
      l_Msg := 'cipks_cidtkf_Main ==>'||p_Msg;
      Debug.Pr_Debug('CI' ,l_Msg);
   END Dbg;

   PROCEDURE Pr_Log_Error(p_Source VARCHAR2,p_Err_Code VARCHAR2, p_Err_Params VARCHAR2) IS
      l_Fid    VARCHAR2(32767) := 'CIDTKF';
   BEGIN
      Cspks_Req_Utils.Pr_Log_Error(p_Source,l_Fid,p_Err_Code,p_Err_Params);
   END Pr_Log_Error;
   PROCEDURE Pr_Skip_Handler(p_Stage in VARCHAR2) IS
   BEGIN
      cipks_cidtkf_Custom.Pr_Skip_Handler (P_Stage);
   END Pr_Skip_Handler;
   PROCEDURE Pr_Set_Skip_Sys IS
   BEGIN
      g_Skip_Sys := TRUE;
   END Pr_Set_Skip_Sys;
   PROCEDURE Pr_Set_Activate_Sys IS
   BEGIN
      g_Skip_Sys := FALSE;
   END Pr_Set_Activate_Sys;
   FUNCTION  Fn_Skip_Sys RETURN BOOLEAN IS
   BEGIN
      RETURN G_Skip_Sys;
   END  Fn_Skip_Sys;
   PROCEDURE Pr_Set_Skip_Custom IS
   BEGIN
      g_Skip_Custom := TRUE;
   END Pr_Set_Skip_Custom;
   PROCEDURE Pr_Set_Activate_Custom IS
   BEGIN
      g_Skip_Custom := FALSE;
   END Pr_Set_Activate_Custom;
   FUNCTION  Fn_Skip_Custom RETURN BOOLEAN IS
   BEGIN
      IF Cspks_Req_Global.g_Release_Type IN(Cspks_Req_Global.p_Kernel,Cspks_Req_Global.P_Cluster) THEN
         RETURN TRUE;
      ELSIF Cspks_Req_Global.g_Release_Type =Cspks_Req_Global.p_Custom THEN
         RETURN FALSE;
      ELSE
         RETURN G_Skip_Custom;
      END IF;
   END  Fn_Skip_Custom;
   FUNCTION Fn_Sys_Build_Fc_Type (p_Source    IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Addl_Info       IN Cspks_Req_Global.Ty_Addl_Info,
      p_cidtkf       IN   OUT cipks_cidtkf_Main.ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS

      l_Pk_counter        NUMBER :=1;
      l_Count             NUMBER;
      l_Parent_Rec        NUMBER :=0;
      l_Key               VARCHAR2(255);
      l_Pkey              VARCHAR2(32767);
      l_PVal              VARCHAR2(32767);
      l_Val               VARCHAR2(32767);
      l_Tag               VARCHAR2(100);
      l_Node              VARCHAR2(100);
      l_Key_Vals          VARCHAR2(32767);
      l_Key_Tags          VARCHAR2(32767);
      l_Source_Operation  VARCHAR2(100) := p_Source_Operation;
      l_Dsn_Rec_Cnt_2    NUMBER;
      l_Bnd_Cntr_2    NUMBER;

   BEGIN

      dbg('In Fn_Sys_Build_Fc_Type..');

      l_Node := Cspks_Req_Global.Fn_GetNode;
      WHILE (l_Node <> 'EOPL')
      LOOP
         --Dbg('Node Name  :'||l_Node);
         IF  l_Node = 'BLK_ACC' THEN
            p_cidtkf.v_cltb_account_apps_master.ACCOUNT_NUMBER := Cspks_Req_Global.Fn_GetVal;
            p_cidtkf.v_cltb_account_apps_master.BRANCH_CODE := Cspks_Req_Global.Fn_GetVal;
         ELSIF  l_Node = 'BLK_TKF' THEN
            l_Dsn_Rec_Cnt_2 :=  p_cidtkf.v_cltb_account_insurance.count +1 ;
            p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).YEARS := Cspks_Req_Global.Fn_GetVal;
            l_Val       := Cspks_Req_Global.Fn_GetVal;
            IF Length(l_Val) > Length(Cspks_Req_Global.g_Date_Format) THEN
               p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).EFFECTIVE_DATE := TO_DATE(l_val,Cspks_Req_Global.g_Date_Time_Format);
            ELSE
               p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).EFFECTIVE_DATE := TO_DATE(l_val,Cspks_Req_Global.g_Date_Format);
            END IF;
            p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).BASIS_AMOUNT := Cspks_Req_Global.Fn_GetVal;
            p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).DEPRECIATION_RATE := Cspks_Req_Global.Fn_GetVal;
            p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).INSURANCE_RATE := Cspks_Req_Global.Fn_GetVal;
            p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).PREMIUM_AMOUNT := Cspks_Req_Global.Fn_GetVal;
            p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).POLICY_NUMBER := Cspks_Req_Global.Fn_GetVal;
            l_Val       := Cspks_Req_Global.Fn_GetVal;
            IF Length(l_Val) > Length(Cspks_Req_Global.g_Date_Format) THEN
               p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).EXPIRY_DATE := TO_DATE(l_val,Cspks_Req_Global.g_Date_Time_Format);
            ELSE
               p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).EXPIRY_DATE := TO_DATE(l_val,Cspks_Req_Global.g_Date_Format);
            END IF;
            p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).account_number :=p_cidtkf.v_cltb_account_apps_master.account_number;
         END IF;
         l_Node := Cspks_Req_Global.Fn_GetNode;
      END LOOP;

      p_cidtkf.Addl_Info := p_Addl_Info;
      Dbg('Returning Success From Fn_Sys_Build_Fc_Type.. ');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of cipks_cidtkf_Main.Fn_Sys_Build_Fc_Type ');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Build_Fc_Type;
   FUNCTION Fn_Sys_Build_Ws_Type (p_Source    IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Addl_Info       IN Cspks_Req_Global.Ty_Addl_Info,
      p_cidtkf       IN   OUT cipks_cidtkf_Main.ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS

      l_Pk_counter        NUMBER :=1;
      l_Count             NUMBER;
      l_Parent_Rec        NUMBER :=0;
      l_Key               VARCHAR2(255);
      l_Pkey              VARCHAR2(32767);
      l_PVal              VARCHAR2(32767);
      l_Val               VARCHAR2(32767);
      l_Tag               VARCHAR2(100);
      l_Node              VARCHAR2(100);
      l_Key_Vals          VARCHAR2(32767);
      l_Key_Tags          VARCHAR2(32767);
      l_Source_Operation  VARCHAR2(100) := p_Source_Operation;
      Invalid_Date        EXCEPTION;
      l_Dsn_Rec_Cnt_2    NUMBER;
      l_Bnd_Cntr_2    NUMBER;

   BEGIN

      dbg('In Fn_Sys_Build_Ws_Type..');

      l_Node := Cspks_Req_Global.Fn_GetNode;
      WHILE (l_Node <> 'EOPL')
      LOOP
         --Dbg('Node Name  :'||l_Node);
         IF  l_Node IN ( 'BLK_ACC','Acc-Full','Acc-IO') THEN
            l_Key       := Cspks_Req_Global.Fn_GetTag;
            l_Val       := Cspks_Req_Global.Fn_GetVal;
            WHILE (l_Key <> 'EOPL')
            LOOP
               --dbg('Key/Value   :'||l_Key ||':'||l_Val);
               IF l_Key = 'ACC' THEN
                  p_cidtkf.v_cltb_account_apps_master.ACCOUNT_NUMBER := l_Val;
               ELSIF l_Key = 'BRN' THEN
                  p_cidtkf.v_cltb_account_apps_master.BRANCH_CODE := l_Val;
               END IF;
               l_Key       := Cspks_Req_Global.Fn_GetTag;
               l_Val       := Cspks_Req_Global.Fn_GetVal;
            END LOOP;
         ELSIF  l_Node IN ( 'BLK_TKF','Tkf') THEN
            l_Dsn_Rec_Cnt_2 :=  p_cidtkf.v_cltb_account_insurance.count +1 ;
            l_Key       := Cspks_Req_Global.Fn_GetTag;
            l_Val       := Cspks_Req_Global.Fn_GetVal;
            WHILE (l_Key <> 'EOPL')
            LOOP
               --dbg('Key/Value   :'||l_Key ||':'||l_Val);
               IF l_Key = 'YEARS' THEN
                  p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).YEARS := l_Val;
               ELSIF l_Key = 'EFF_DT' THEN
                  BEGIN
                     IF Length(l_Val) > Length(Cspks_Req_Global.g_Date_Format) THEN
                        p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).EFFECTIVE_DATE := TO_DATE(l_val,Cspks_Req_Global.g_Date_Time_Format);
                     ELSE
                        p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).EFFECTIVE_DATE := TO_DATE(l_val,Cspks_Req_Global.g_Date_Format);
                     END IF;
                  EXCEPTION
                     WHEN OTHERS THEN
                        RAISE Invalid_Date;
                  END;
               ELSIF l_Key = 'BASIS_AMT' THEN
                  p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).BASIS_AMOUNT := l_Val;
               ELSIF l_Key = 'DEP_RATE' THEN
                  p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).DEPRECIATION_RATE := l_Val;
               ELSIF l_Key = 'INSR_RATE' THEN
                  p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).INSURANCE_RATE := l_Val;
               ELSIF l_Key = 'PREM_AMT' THEN
                  p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).PREMIUM_AMOUNT := l_Val;
               ELSIF l_Key = 'POL_NUM' THEN
                  p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).POLICY_NUMBER := l_Val;
               ELSIF l_Key = 'EXP_DT' THEN
                  BEGIN
                     IF Length(l_Val) > Length(Cspks_Req_Global.g_Date_Format) THEN
                        p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).EXPIRY_DATE := TO_DATE(l_val,Cspks_Req_Global.g_Date_Time_Format);
                     ELSE
                        p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).EXPIRY_DATE := TO_DATE(l_val,Cspks_Req_Global.g_Date_Format);
                     END IF;
                  EXCEPTION
                     WHEN OTHERS THEN
                        RAISE Invalid_Date;
                  END;
               END IF;
               l_Key       := Cspks_Req_Global.Fn_GetTag;
               l_Val       := Cspks_Req_Global.Fn_GetVal;
            END LOOP;
            p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).account_number :=p_cidtkf.v_cltb_account_apps_master.account_number;
         END IF;
         l_Node := Cspks_Req_Global.Fn_GetNode;
      END LOOP;

      p_cidtkf.Addl_Info := p_Addl_Info;
      Dbg('Returning Success From Fn_Sys_Build_Fc_Type.. ');
      RETURN TRUE;

   EXCEPTION
      WHEN Invalid_Date THEN
         Pr_Log_Error(p_Source,'ST-OTHR-003',l_Key||'~'||Cspks_Req_Global.g_Date_Format) ;
         RETURN FALSE;
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of cipks_cidtkf_Main.Fn_Sys_Build_Fc_Type ');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Build_Ws_Type;
   FUNCTION Fn_Sys_Build_Fc_Ts (p_Source    IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_cidtkf          IN cipks_cidtkf_Main.ty_cidtkf,
      p_Err_Code        IN OUT VARCHAR2,
      p_Err_Params      IN OUT VARCHAR2)
   RETURN BOOLEAN   IS
      l_Level_Format  VARCHAR2(32767);
      l_Parent_Format VARCHAR2(32767);
      l_Date_Val      VARCHAR2(32767);
      l_Master_Childs NUMBER := 0;
      l_Desc_Vc          VARCHAR2(32767);
      l_Source_Operation       VARCHAR2(100) := p_Source_Operation;
      l_0_Lvl_Counter NUMBER := 0;
      l_1_Lvl_Counter NUMBER := 0;
      l_2_Lvl_Counter   NUMBER := 0;
      l_Dsn_Rec_Cnt_2    NUMBER;
      l_Bnd_Cntr_2    NUMBER;
      l_Cntr_Before   NUMBER := 0;
      l_Master_Where  VARCHAR2(32767);
      l_Count         NUMBER := 0;

   BEGIN
      Dbg('In Fn_Sys_Build_Fc_Ts..');

      --Dbg('Building Childs Of :..');
      l_1_Lvl_Counter := 0;
      l_0_Lvl_Counter   := l_0_Lvl_Counter +1;
      l_Level_Format      := l_0_Lvl_Counter;
      Cspks_Req_Global.Pr_Write('P','BLK_ACC',l_Level_Format);
      Cspks_Req_Global.Pr_Write('V','ACC',p_cidtkf.v_cltb_account_apps_master.account_number);
      Cspks_Req_Global.Pr_Write('V','BRN',p_cidtkf.v_cltb_account_apps_master.branch_code);

      --Dbg('Building Childs Of :BLK_ACC..');
      l_Dsn_Rec_Cnt_2 := 0;
      IF p_cidtkf.v_cltb_account_insurance.COUNT > 0 THEN
         FOR i_2 IN  1..p_cidtkf.v_cltb_account_insurance.COUNT LOOP
            l_Dsn_Rec_Cnt_2 := i_2;
            l_Master_Childs  :=  l_Master_Childs +1;
            l_1_Lvl_Counter   := l_1_Lvl_Counter +1;
            l_Level_Format      := l_0_Lvl_Counter||'.'||l_1_Lvl_Counter;
            Cspks_Req_Global.Pr_Write('P','BLK_TKF',l_Level_Format);
            Cspks_Req_Global.Pr_Write('V','YEARS',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).years);
            IF trunc(p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).effective_date) <>
                  p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).effective_date THEN
               l_Date_Val :=  TO_CHAR( p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).effective_date,Cspks_Req_Global.g_Ws_Date_Time_Format);
            ELSE
               l_Date_Val :=  TO_CHAR( p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).effective_date,Cspks_Req_Global.g_Ws_Date_Format);
            END IF;
            Cspks_Req_Global.Pr_Write('V','EFF_DT',l_Date_Val);
            Cspks_Req_Global.Pr_Write('V','BASIS_AMT',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).basis_amount);
            Cspks_Req_Global.Pr_Write('V','DEP_RATE',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).depreciation_rate);
            Cspks_Req_Global.Pr_Write('V','INSR_RATE',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).insurance_rate);
            Cspks_Req_Global.Pr_Write('V','PREM_AMT',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).premium_amount);
            Cspks_Req_Global.Pr_Write('V','POL_NUM',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).policy_number);
            IF trunc(p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).expiry_date) <>
                  p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).expiry_date THEN
               l_Date_Val :=  TO_CHAR( p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).expiry_date,Cspks_Req_Global.g_Ws_Date_Time_Format);
            ELSE
               l_Date_Val :=  TO_CHAR( p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).expiry_date,Cspks_Req_Global.g_Ws_Date_Format);
            END IF;
            Cspks_Req_Global.Pr_Write('V','EXP_DT',l_Date_Val);
         END LOOP;
      END IF;
      Dbg('Returning Success From Fn_Sys_Build_Fc_Ts..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Build_Fc_Ts..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Build_Fc_Ts;
   FUNCTION Fn_Sys_Build_Ws_Ts (p_Source    IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Exchange_Pattern IN       VARCHAR2,
      p_cidtkf          IN cipks_cidtkf_Main.ty_cidtkf,
      p_Err_Code        IN OUT VARCHAR2,
      p_Err_Params      IN OUT VARCHAR2)
   RETURN BOOLEAN   IS
      l_Level_Format  VARCHAR2(32767);
      l_Parent_Format VARCHAR2(32767);
      l_Date_Val      VARCHAR2(32767);
      l_Master_Childs NUMBER := 0;
      l_Desc_Vc          VARCHAR2(32767);
      l_Source_Operation       VARCHAR2(100) := p_Source_Operation;
      l_Key_Cols          VARCHAR2(32767);
      l_Key_Vals          VARCHAR2(32767);
      l_0_Lvl_Counter NUMBER := 0;
      l_1_Lvl_Counter NUMBER := 0;
      l_2_Lvl_Counter   NUMBER := 0;
      l_Dsn_Rec_Cnt_2    NUMBER;
      l_Bnd_Cntr_2    NUMBER;
      l_Cntr_Before   NUMBER := 0;
      l_Master_Where  VARCHAR2(32767);
      l_Count         NUMBER := 0;

   BEGIN
      Dbg('In Fn_Sys_Build_Ws_Ts..');
      IF SUBSTR(p_Exchange_Pattern,3,4) = 'FS' THEN
         Dbg('Building Full Screen Reply..');

         --Dbg('Building Childs Of :..');
         IF (  p_cidtkf.v_cltb_account_apps_master.account_number IS NOT NULL 
         AND  p_cidtkf.v_cltb_account_apps_master.branch_code IS NOT NULL 
          )
          THEN
            l_1_Lvl_Counter := 0;
            l_0_Lvl_Counter   := l_0_Lvl_Counter +1;
            l_Level_Format      := l_0_Lvl_Counter;
            Cspks_Req_Global.Pr_Write('P','Acc-Full',l_Level_Format);
            Cspks_Req_Global.Pr_Write('V','ACC',p_cidtkf.v_cltb_account_apps_master.account_number);
            Cspks_Req_Global.Pr_Write('V','BRN',p_cidtkf.v_cltb_account_apps_master.branch_code);

            --Dbg('Building Childs Of :BLK_ACC..');
            l_Dsn_Rec_Cnt_2 := 0;
            IF p_cidtkf.v_cltb_account_insurance.COUNT > 0 THEN
               FOR i_2 IN  1..p_cidtkf.v_cltb_account_insurance.COUNT LOOP
                  l_Dsn_Rec_Cnt_2 := i_2;
                  l_Master_Childs  :=  l_Master_Childs +1;
                  l_1_Lvl_Counter   := l_1_Lvl_Counter +1;
                  l_Level_Format      := l_0_Lvl_Counter||'.'||l_1_Lvl_Counter;
                  Cspks_Req_Global.Pr_Write('P','Tkf',l_Level_Format);
                  Cspks_Req_Global.Pr_Write('V','YEARS',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).years);
                  IF trunc(p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).effective_date) <>
                        p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).effective_date THEN
                     l_Date_Val :=  TO_CHAR( p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).effective_date,Cspks_Req_Global.g_Ws_Date_Time_Format);
                  ELSE
                     l_Date_Val :=  TO_CHAR( p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).effective_date,Cspks_Req_Global.g_Ws_Date_Format);
                  END IF;
                  Cspks_Req_Global.Pr_Write('V','EFF_DT',l_Date_Val);
                  Cspks_Req_Global.Pr_Write('V','BASIS_AMT',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).basis_amount);
                  Cspks_Req_Global.Pr_Write('V','DEP_RATE',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).depreciation_rate);
                  Cspks_Req_Global.Pr_Write('V','INSR_RATE',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).insurance_rate);
                  Cspks_Req_Global.Pr_Write('V','PREM_AMT',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).premium_amount);
                  Cspks_Req_Global.Pr_Write('V','POL_NUM',p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).policy_number);
                  IF trunc(p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).expiry_date) <>
                        p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).expiry_date THEN
                     l_Date_Val :=  TO_CHAR( p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).expiry_date,Cspks_Req_Global.g_Ws_Date_Time_Format);
                  ELSE
                     l_Date_Val :=  TO_CHAR( p_cidtkf.v_cltb_account_insurance(l_Dsn_Rec_Cnt_2).expiry_date,Cspks_Req_Global.g_Ws_Date_Format);
                  END IF;
                  Cspks_Req_Global.Pr_Write('V','EXP_DT',l_Date_Val);
               END LOOP;
            END IF;
         END IF;
      ELSE
         Dbg('Building Primary Key Reply..');
         Cspks_Req_Global.pr_Write('P','Acc-PK','1');
         l_Key_Cols := 'ACC~'||'BRN~';
         l_Key_Vals := p_cidtkf.v_cltb_account_apps_master.account_number||'~'||p_cidtkf.v_cltb_account_apps_master.branch_code||'~';
         Cspks_Req_Global.pr_Write('V',l_Key_Cols,l_Key_Vals);
      END IF;
      Dbg('Returning Success From Fn_Sys_Build_Ws_Ts..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Build_Fc_Ts..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Build_Ws_Ts;
   FUNCTION Fn_Sys_Check_Mandatory (p_Source    IN  VARCHAR2,
      p_Pk_Or_Full     IN  VARCHAR2 DEFAULT 'FULL',
      p_cidtkf IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code       IN  OUT VARCHAR2,
      p_Err_Params     IN  OUT VARCHAR2)
   RETURN BOOLEAN IS

      l_Count          NUMBER:= 0;
      l_Key            VARCHAR2(5000);
      l_Blk            VARCHAR2(100);
      l_Fld            VARCHAR2(100);
      l_Rec_Sent       BOOLEAN := TRUE;
      l_Base_Data_From_Fc  VARCHAR2(1):= 'Y';

   BEGIN

      Dbg('In Fn_Sys_Check_Mandatory..');

      l_Fld := 'CLTB_ACCOUNT_APPS_MASTER.ACCOUNT_NUMBER';
      IF p_cidtkf.v_cltb_account_apps_master.account_number IS Null THEN
         Dbg('Field account_number is Null..');
         p_Err_Code    := 'ST-MAND-001';
         p_Err_Params := '@'||l_Fld;
         RETURN FALSE;
      END IF;
      l_Fld := 'CLTB_ACCOUNT_APPS_MASTER.BRANCH_CODE';
      IF p_cidtkf.v_cltb_account_apps_master.branch_code IS Null THEN
         Dbg('Field branch_code is Null..');
         p_Err_Code    := 'ST-MAND-001';
         p_Err_Params := '@'||l_Fld;
         RETURN FALSE;
      END IF;

      IF p_Pk_Or_Full = 'FULL'  THEN
         Dbg('Full Mandatory Checks..');

         l_Blk := 'CLTB_ACCOUNT_APPS_MASTER';

         l_Blk := 'CLTB_ACCOUNT_INSURANCE';
         l_Count := p_cidtkf.v_cltb_account_insurance.COUNT;
         IF l_Count > 0 THEN
            FOR l_index IN 1 .. p_cidtkf.v_cltb_account_insurance.COUNT LOOP
               l_Fld := 'CLTB_ACCOUNT_INSURANCE.YEARS';
               IF p_cidtkf.v_cltb_account_insurance(l_Index).years IS Null THEN
                  Dbg('Primary Key Column years Cannot Be Null');
                  l_Key := Null;
                  Pr_Log_Error(p_Source,'ST-MAND-003','@'||l_Fld||'~@'||l_Blk||'~'||l_index );
               END IF;
               l_Key := nvl(l_Key, to_char(l_index));
            END LOOP;
         END IF;
      END IF;

      Dbg('Returning Success From Fn_Sys_Check_Mandatory ..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_debug('**','In When Others of Fn_Sys_Check_Mandatory ..');
         Debug.Pr_debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := Null;
         RETURN FALSE;
   END Fn_Sys_Check_Mandatory;
   FUNCTION Fn_Sys_Basic_Vals        (p_Source            IN VARCHAR2,
      p_cidtkf     IN  cipks_cidtkf_Main.ty_cidtkf,
      p_Err_code          IN OUT VARCHAR2,
      p_Err_params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS

      l_Count          NUMBER:= 0;
      l_Key            VARCHAR2(5000):= NULL;
      i                NUMBER := 1;
      l_Blk            VARCHAR2(100):= 0;
      l_Fld            VARCHAR2(100):= 0;
      l_Inv_Chr        VARCHAR2(5) :=NULL;
   BEGIN

      Dbg('In Fn_Sys_Basic_Vals..');
      Dbg('Duplicate Records Check For :v_cltb_account_insurance..');
      l_Count      := p_cidtkf.v_cltb_account_insurance.COUNT;
      IF l_Count > 0 THEN
         FOR l_index  IN 1 .. l_count LOOP
            l_key := NULL;
            IF l_index < l_Count THEN
               FOR l_index1 IN l_index+1 .. l_Count LOOP
                  IF (NVL(p_cidtkf.v_cltb_account_insurance(l_index).account_number,'@')=  NVL(p_cidtkf.v_cltb_account_insurance(l_index1).account_number,'@')) AND (NVL(p_cidtkf.v_cltb_account_insurance(l_index).years,-1)=  NVL(p_cidtkf.v_cltb_account_insurance(l_index1).years,-1)) THEN
                     Dbg('Duplicare Record Found for :'||l_Key);
                     l_key := Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_INSURANCE.ACCOUNT_NUMBER')||'-'||
                     p_cidtkf.v_cltb_account_insurance(l_index).account_number||':'||
                     Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_INSURANCE.YEARS')||'-'||
                     p_cidtkf.v_cltb_account_insurance(l_index).years;
                     Pr_Log_Error(p_Source,'ST-VALS-009','@CLTB_ACCOUNT_INSURANCE~'||l_Key);
                  END IF;
               END LOOP;
            END IF;
         END LOOP;
      END IF;
      Dbg('Returning Success From Fn_Sys_Basic_Vals..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Basic_Vals..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Basic_Vals;

   FUNCTION Fn_Sys_Default_Vals        (p_Source            IN VARCHAR2,
      p_Wrk_cidtkf     IN  OUT cipks_cidtkf_Main.ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS

      l_Count          NUMBER:= 0;
   BEGIN

      Dbg('In Fn_Sys_Default_Vals..');
      Dbg('Returning Success From Fn_Sys_Default_Vals..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Default_Vals..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Default_Vals;

   FUNCTION Fn_Sys_Merge_Amendables        (p_Source            IN VARCHAR2,
      p_Source_Operation  IN     VARCHAR2,
      p_cidtkf     IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Prev_cidtkf IN cipks_cidtkf_Main.Ty_cidtkf,
      p_Wrk_cidtkf IN OUT cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS

      l_Count          NUMBER:= 0;
      l_Wrk_Count      NUMBER := 0 ;
      l_Deleted_Recs   NUMBER := 0;
      l_Modified_Flds  VARCHAR2(32000):= NULL;
      l_Key            VARCHAR2(5000):= NULL;
      l_Mod_Fld        VARCHAR2(100):= NULL;
      i                NUMBER := 1;
      l_Rec_Found      BOOLEAN := FALSE;
      l_Rec_Modified   BOOLEAN := FALSE;
      l_Rec_Sent       BOOLEAN := FALSE;
      l_Blk            VARCHAR2(100):= 0;
      l_Fld            VARCHAR2(100):= 0;
      l_Pk_Or_Full     VARCHAR2(5) :='FULL';
      l_Inv_Chr        VARCHAR2(5) :=NULL;
      l_Mod_No         NUMBER:= 0;
      l_Base_Data_From_Fc  VARCHAR2(1):= 'Y';
      l_Amendable_Nodes Cspks_Req_Global.Ty_Amend_Nodes;
      l_Amendable_Fields Cspks_Req_Global.Ty_Amend_Fields;
      N_v_cltb_account_insurance       cipks_cidtkf_Main.Ty_Tb_v_cltb_account_insurance;

      FUNCTION Fn_Amendable(p_Item IN VARCHAR2) RETURN BOOLEAN IS
      BEGIN
         IF l_Amendable_Fields.EXISTS(p_Item) THEN
            RETURN TRUE;
         ELSE
            RETURN FALSE;
         END IF;
      END Fn_Amendable;
   BEGIN

      Dbg('In Fn_Sys_Merge_Amendables');

      Dbg('Calling Cspks_Req_Utils.Fn_Get_Amendable_Details..');
      IF NOT Cspks_Req_Utils.Fn_Get_Amendable_Details(p_source ,
         p_Source_Operation,
         l_Amendable_Nodes,
         l_Amendable_Fields,
         p_Err_Code,
         p_Err_Params) THEN
         Dbg('Failed in Cspks_Req_Utils.Fn_Get_Amendable_Details..');
         Pr_Log_Error(p_Source,p_Err_Code,p_Err_Params);
      END IF;

      l_Blk := 'CLTB_ACCOUNT_APPS_MASTER';
      l_Rec_Modified := FALSE;
      l_Modified_Flds  := NULL;


      l_Modified_Flds := LTRIM(l_Modified_Flds,'~');
      IF  l_Rec_Modified THEN
         IF l_Modified_Flds IS NOT NULL THEN
            i :=  1;
            l_Mod_Fld := Cspkes_Misc.fn_GetParam(l_modified_flds,i,'~');
            WHILE l_Mod_Fld <> 'EOPL' LOOP
               Pr_Log_Error(p_Source,'ST-AMND-002','@'||l_Mod_Fld||'~@'||l_Blk) ;
               i := i +1;
               l_Mod_Fld := Cspkes_Misc.fn_GetParam(l_Modified_Flds,i,'~');
            END LOOP;
         END IF;
      END IF;
      l_Blk := 'CLTB_ACCOUNT_INSURANCE';
      l_Count      := p_cidtkf.v_cltb_account_insurance.COUNT;
      l_Wrk_Count  := p_Wrk_cidtkf.v_cltb_account_insurance.COUNT;
      IF l_Count > 0 THEN
         FOR l_index IN 1..l_Count  LOOP
            l_Rec_Found := FALSE;
            l_Rec_Modified := FALSE;
            l_Modified_Flds := NULL;
            IF l_Wrk_Count > 0 THEN
               FOR l_index1 IN 1..l_Wrk_Count  LOOP
                  IF (NVL(p_cidtkf.v_cltb_account_insurance(l_index).years,-1)=  NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).years,-1)) THEN
                     Dbg('Record Found..');
                     l_Rec_Found := TRUE;
                     l_fld := 'CLTB_ACCOUNT_INSURANCE.EFFECTIVE_DATE';
                     IF Fn_Amendable('CLTB_ACCOUNT_INSURANCE.EFFECTIVE_DATE') THEN
                        p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).effective_date := p_cidtkf.v_cltb_account_insurance(l_index).effective_date;
                     ELSE
                        IF p_cidtkf.v_cltb_account_insurance(l_index).effective_date IS NOT NULL THEN
                           IF NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).effective_date,global.min_date) <>
                              NVL(p_cidtkf.v_cltb_account_insurance(l_index).effective_date,global.min_date)  THEN
                              l_Modified_flds := l_Modified_Flds ||'~'||l_Fld;
                              l_Rec_Modified  := TRUE;
                           END IF;
                        END IF;
                     END IF;
                     l_fld := 'CLTB_ACCOUNT_INSURANCE.BASIS_AMOUNT';
                     IF Fn_Amendable('CLTB_ACCOUNT_INSURANCE.BASIS_AMOUNT') THEN
                        p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).basis_amount := p_cidtkf.v_cltb_account_insurance(l_index).basis_amount;
                     ELSE
                        IF p_cidtkf.v_cltb_account_insurance(l_index).basis_amount IS NOT NULL THEN
                           IF NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).basis_amount,-1) <>
                              NVL(p_cidtkf.v_cltb_account_insurance(l_index).basis_amount,-1)  THEN
                              l_Modified_flds := l_Modified_Flds ||'~'||l_Fld;
                              l_Rec_Modified  := TRUE;
                           END IF;
                        END IF;
                     END IF;
                     l_fld := 'CLTB_ACCOUNT_INSURANCE.DEPRECIATION_RATE';
                     IF Fn_Amendable('CLTB_ACCOUNT_INSURANCE.DEPRECIATION_RATE') THEN
                        p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).depreciation_rate := p_cidtkf.v_cltb_account_insurance(l_index).depreciation_rate;
                     ELSE
                        IF p_cidtkf.v_cltb_account_insurance(l_index).depreciation_rate IS NOT NULL THEN
                           IF NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).depreciation_rate,-1) <>
                              NVL(p_cidtkf.v_cltb_account_insurance(l_index).depreciation_rate,-1)  THEN
                              l_Modified_flds := l_Modified_Flds ||'~'||l_Fld;
                              l_Rec_Modified  := TRUE;
                           END IF;
                        END IF;
                     END IF;
                     l_fld := 'CLTB_ACCOUNT_INSURANCE.INSURANCE_RATE';
                     IF Fn_Amendable('CLTB_ACCOUNT_INSURANCE.INSURANCE_RATE') THEN
                        p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).insurance_rate := p_cidtkf.v_cltb_account_insurance(l_index).insurance_rate;
                     ELSE
                        IF p_cidtkf.v_cltb_account_insurance(l_index).insurance_rate IS NOT NULL THEN
                           IF NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).insurance_rate,-1) <>
                              NVL(p_cidtkf.v_cltb_account_insurance(l_index).insurance_rate,-1)  THEN
                              l_Modified_flds := l_Modified_Flds ||'~'||l_Fld;
                              l_Rec_Modified  := TRUE;
                           END IF;
                        END IF;
                     END IF;
                     l_fld := 'CLTB_ACCOUNT_INSURANCE.PREMIUM_AMOUNT';
                     IF Fn_Amendable('CLTB_ACCOUNT_INSURANCE.PREMIUM_AMOUNT') THEN
                        p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).premium_amount := p_cidtkf.v_cltb_account_insurance(l_index).premium_amount;
                     ELSE
                        IF p_cidtkf.v_cltb_account_insurance(l_index).premium_amount IS NOT NULL THEN
                           IF NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).premium_amount,-1) <>
                              NVL(p_cidtkf.v_cltb_account_insurance(l_index).premium_amount,-1)  THEN
                              l_Modified_flds := l_Modified_Flds ||'~'||l_Fld;
                              l_Rec_Modified  := TRUE;
                           END IF;
                        END IF;
                     END IF;
                     l_fld := 'CLTB_ACCOUNT_INSURANCE.POLICY_NUMBER';
                     IF Fn_Amendable('CLTB_ACCOUNT_INSURANCE.POLICY_NUMBER') THEN
                        p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).policy_number := p_cidtkf.v_cltb_account_insurance(l_index).policy_number;
                     ELSE
                        IF p_cidtkf.v_cltb_account_insurance(l_index).policy_number IS NOT NULL THEN
                           IF NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).policy_number,'@') <>
                              NVL(p_cidtkf.v_cltb_account_insurance(l_index).policy_number,'@')  THEN
                              l_Modified_flds := l_Modified_Flds ||'~'||l_Fld;
                              l_Rec_Modified  := TRUE;
                           END IF;
                        END IF;
                     END IF;
                     l_fld := 'CLTB_ACCOUNT_INSURANCE.EXPIRY_DATE';
                     IF Fn_Amendable('CLTB_ACCOUNT_INSURANCE.EXPIRY_DATE') THEN
                        p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).expiry_date := p_cidtkf.v_cltb_account_insurance(l_index).expiry_date;
                     ELSE
                        IF p_cidtkf.v_cltb_account_insurance(l_index).expiry_date IS NOT NULL THEN
                           IF NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).expiry_date,global.min_date) <>
                              NVL(p_cidtkf.v_cltb_account_insurance(l_index).expiry_date,global.min_date)  THEN
                              l_Modified_flds := l_Modified_Flds ||'~'||l_Fld;
                              l_Rec_Modified  := TRUE;
                           END IF;
                        END IF;
                     END IF;

                     l_Modified_Flds := LTRIM(l_Modified_Flds,'~');
                     IF  l_Rec_modified THEN
                        IF l_Modified_Flds IS NOT NULL THEN
                           l_key := Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_INSURANCE.YEARS')||'-'||
                           p_cidtkf.v_cltb_account_insurance(l_index).years;
                           i :=  1;
                           l_Mod_Fld := Cspkes_Misc.Fn_GetParam(l_Modified_Flds,i,'~');
                           WHILE l_mod_fld <> 'EOPL' LOOP
                              Pr_Log_Error(p_Source,'ST-AMND-003','@'||l_Mod_Fld||'~@'||l_Blk||'~'||l_Key );
                              i := i +1;
                              l_Mod_Fld := Cspkes_Misc.Fn_GetParam(l_Modified_Flds,i,'~');
                           END LOOP;
                        END IF;
                     END IF;
                  END IF;
               END LOOP;
            END IF;
            IF NOT l_Rec_Found THEN
               p_Wrk_cidtkf.v_cltb_account_insurance(p_Wrk_cidtkf.v_cltb_account_insurance.COUNT +1 ) :=  p_cidtkf.v_cltb_account_insurance(l_index);
               IF l_Amendable_Nodes.EXISTS('CLTB_ACCOUNT_INSURANCE') THEN
                  IF l_Amendable_Nodes('CLTB_ACCOUNT_INSURANCE').New_Allowed = 'N' THEN
                     Dbg('New Record Cannot Be Added..');
                     l_key := Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_INSURANCE.YEARS')||'-'||
                     p_cidtkf.v_cltb_account_insurance(l_index).years;
                     Pr_Log_Error(p_source,'ST-AMND-004',l_key||'~@'||l_blk);
                  END IF;
               ELSE
                  Dbg('New Record Cannot Be Added..');
                  l_key := Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_INSURANCE.YEARS')||'-'||
                  p_cidtkf.v_cltb_account_insurance(l_index).years;
                  Pr_Log_Error(p_Source,'ST-AMND-004',l_key||'~@'||l_blk);
               END IF;
            END IF;
         END LOOP;
      END IF;

      IF l_Amendable_Nodes.EXISTS('CLTB_ACCOUNT_INSURANCE') THEN
         IF l_Amendable_Nodes('CLTB_ACCOUNT_INSURANCE').All_Records = 'Y' THEN
            Dbg('Logic For Deleting Some Records From Work Record  if Not sent..');
            l_Wrk_Count := p_Wrk_cidtkf.v_cltb_account_insurance.COUNT;
            l_Count     := p_cidtkf.v_cltb_account_insurance.COUNT;
            IF l_Wrk_Count > 0 THEN
               FOR l_index1 IN 1..l_Wrk_count  LOOP
                  l_Rec_Found := FALSE;
                  IF l_Count > 0 THEN
                     FOR l_index IN 1..l_Count  LOOP
                        IF (NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).years,-1)=  NVL(p_cidtkf.v_cltb_account_insurance  (l_index).years,-1)) THEN
                           Dbg('Record Found..');
                           l_Rec_Found := TRUE;
                           EXIT;
                        END IF;
                     END LOOP;
                  END IF;
                  IF l_Rec_Found THEN
                     Dbg('Adding  a Record...');
                     N_v_cltb_account_insurance(N_v_cltb_account_insurance.COUNT +1 ) :=  p_Wrk_cidtkf.v_cltb_account_insurance(l_Index1);
                  ELSE
                     l_Deleted_Recs := l_Deleted_Recs +1;
                     IF l_Amendable_Nodes.EXISTS('CLTB_ACCOUNT_INSURANCE') THEN
                        IF l_Amendable_Nodes('CLTB_ACCOUNT_INSURANCE').Delete_Allowed = 'N' THEN
                           Dbg('Record Cannot Be Deleted..');
                           l_key := Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_INSURANCE.YEARS')||'-'||
                           p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).years;
                           Pr_Log_Error(p_Source,'ST-AMND-006',l_Key||'~@'||l_Blk);
                        END IF;
                     ELSE
                        Dbg('Record Cannot Be Deleted..');
                        l_key := Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_INSURANCE.YEARS')||'-'||
                        p_Wrk_cidtkf.v_cltb_account_insurance(l_index1).years;
                        Pr_Log_Error(p_Source,'ST-AMND-006',l_Key||'~@'||l_Blk);
                     END IF;
                  END IF;
               END LOOP;
            END IF;
            p_Wrk_cidtkf.v_cltb_account_insurance:= N_v_cltb_account_insurance;
         END IF;
      END IF;

      Dbg('Returning Success From Fn_Sys_Merge_Amendables..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Merge_Amendables..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Merge_Amendables;

   FUNCTION Fn_Sys_Check_Mandatory_Nodes  (p_Source            IN VARCHAR2,
      p_Wrk_cidtkf IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS

      l_Count          NUMBER:= 0;
      l_Base_Data_From_Fc  VARCHAR2(1):= 'Y';
      l_Blk            VARCHAR2(100);
      l_Fld            VARCHAR2(100);
   BEGIN

      dbg('In Fn_Gen_Sys_Node_Mand_Checks..');
      Dbg('Returning Success From Fn_Sys_Check_Mandatory_Nodes..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Check_Mandatory_Nodes..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Check_Mandatory_Nodes;

   FUNCTION Fn_Sys_Lov_Vals        (p_Source            IN VARCHAR2,
      p_Wrk_cidtkf     IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS

      l_Count          NUMBER:= 0;
      l_Key            VARCHAR2(5000):= NULL;
      i                NUMBER := 1;
      l_Lov_Count      NUMBER := 0;
      l_Blk            VARCHAR2(100):= 0;
      l_Fld            VARCHAR2(100):= 0;
      l_Inv_Chr        VARCHAR2(5) :=NULL;
      l_Dsn_Rec_Cnt_1 NUMBER := 0;
      l_Bnd_Cntr_1    NUMBER  := 0;
      l_Dsn_Rec_Cnt_2 NUMBER := 0;
      l_Bnd_Cntr_2    NUMBER  := 0;
   BEGIN

      Dbg('In Fn_Sys_Lov_Vals');
      Dbg('Returning Success From Fn_Sys_Lov_Vals..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Lov_Vals..');
         Debug.Pr_debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Lov_Vals;

   FUNCTION Fn_Sys_Default_And_Validate        (p_Source            IN VARCHAR2,
      p_Source_Operation  IN     VARCHAR2,
      p_Function_id       IN     VARCHAR2,
      p_Action_Code       IN     VARCHAR2,
      p_cidtkf     IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Prev_cidtkf IN OUT  cipks_cidtkf_Main.Ty_cidtkf,
      p_Wrk_cidtkf IN OUT  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS
      l_Base_Data_From_Fc  VARCHAR2(1):= 'Y';
      l_Key  VARCHAR2(32767);
      l_Fld  VARCHAR2(32767);


   BEGIN

      Dbg('In Fn_Sys_Default_and_Validate..');

      IF p_Source <> 'FLEXCUBE'  THEN
         BEGIN
            SELECT Base_Data_From_Fc
            INTO   l_Base_Data_From_Fc
            FROM   Cotms_Source
            WHERE  Source_Code = p_Source;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               Dbg('Failed in Selecting Source '||p_Source);
               Dbg(SQLERRM);
               p_Err_Code    := 'ST-VALS-002';
               p_Err_Params  := p_Source;
               RETURN FALSE;
         END;
      END IF;
      l_key := Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_APPS_MASTER.ACCOUNT_NUMBER')||'-'||
      p_cidtkf.v_cltb_account_apps_master.account_number||':'||
      Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_APPS_MASTER.BRANCH_CODE')||'-'||
      p_cidtkf.v_cltb_account_apps_master.branch_code;
      IF p_Action_Code = Cspks_Req_Global.p_New THEN
         IF p_Prev_cidtkf.v_cltb_account_apps_master.account_number IS NOT NULL THEN
            Dbg('Record Already Exists..');
            Pr_Log_Error(p_Source,'ST-VALS-001',l_Key) ;
         END IF;
      ELSIF p_Action_Code IN (Cspks_Req_Global.p_New,Cspks_Req_Global.p_Modify,Cspks_Req_Global.p_Auth,Cspks_Req_Global.p_Close,Cspks_Req_Global.p_Reopen,Cspks_Req_Global.p_Query,Cspks_Req_Global.p_Delete) THEN
         IF p_Prev_cidtkf.v_cltb_account_apps_master.account_number IS NULL THEN
            Dbg('Record Does not Exist..');
            pr_log_error(p_source,'ST-VALS-002',l_key) ;
         END IF;
      END IF;
      p_Wrk_cidtkf := p_cidtkf;
      IF p_Action_Code in  (Cspks_Req_Global.p_New,Cspks_Req_Global.p_Modify) THEN
         Dbg('Calling .Fn_Sys_Basic_Vals..');
         IF NOT Fn_Sys_Basic_Vals(p_Source,
            p_cidtkf,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in .Fn_Sys_Basic_Vals..');
            RETURN FALSE;
         END IF;

         IF p_Action_Code = Cspks_Req_Global.p_New  THEN
            Dbg('Calling .Fn_Sys_Default_Vals..');
            IF NOT Fn_Sys_Default_Vals(p_Source,
               p_Wrk_cidtkf,
               p_Err_Code,
               p_Err_Params)  THEN
               Dbg('Failed in .Fn_Sys_Default_Vals..');
               RETURN FALSE;
            END IF;

         END IF;
         Dbg('Calling .Fn_Sys_Check_Mandatory_Nodes..');
         IF NOT Fn_Sys_Check_Mandatory_Nodes(p_source,
            p_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in .Fn_Sys_Check_Mandatory_Nodes..');
            RETURN FALSE;
         END IF;

         Dbg('Calling  .Fn_Sys_Lov_Vals..');
         IF NOT Fn_Sys_Lov_Vals(p_source,
            p_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in .Fn_Sys_Lov_Vals..');
            RETURN FALSE;
         END IF;

      END IF;
      Dbg('Returning Success  From Fn_Sys_Default_And_Validate..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of cipks_cidtkf_Main.Fn_Sys_Default_And_Validate ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Default_And_Validate;
   FUNCTION Fn_Sys_Query_Desc_Fields  ( p_Source    IN  VARCHAR2,
                              p_Source_operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Wrk_cidtkf  IN   OUT cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS
      l_Key            VARCHAR2(5000):= NULL;
      l_Count          NUMBER := 0;
      l_Key_Tags       VARCHAR2(32767);
      l_Key_Vals       VARCHAR2(32767);
      l_Rec_Exists     BOOLEAN := TRUE;
      l_Dsn_Rec_Cnt_2 NUMBER := 0;
      l_Bnd_Cntr_2    NUMBER := 0;
   BEGIN
      Dbg('In Fn_Sys_Query_Desc_Fields..');
      Dbg('Returning Success From Fn_Sys_Query_Desc_Fields..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Query_Desc_Fields ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Query_Desc_Fields;
   FUNCTION Fn_Sys_Query  ( p_Source    IN  VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Full_Data     IN  VARCHAR2 DEFAULT 'Y',
      p_With_Lock     IN  VARCHAR2 DEFAULT 'N',
      p_QryData_Reqd       IN  VARCHAR2,
      p_cidtkf         IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Wrk_cidtkf  IN   OUT cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS
      l_Key            VARCHAR2(5000):= NULL;
      l_Count          NUMBER := 0;
      l_Wrk_Count          NUMBER := 0;
      l_Key_Tags       VARCHAR2(32767);
      l_Key_Vals       VARCHAR2(32767);
      l_Rec_Exists        BOOLEAN := TRUE;
      RECORD_LOCKED    EXCEPTION;
      PRAGMA EXCEPTION_INIT( RECORD_LOCKED, -54 );
      l_Dsn_Rec_Cnt_1 NUMBER := 0;
      l_Bnd_Cntr_1    NUMBER := 0;
      l_Dsn_Rec_Cnt_2 NUMBER := 0;
      l_Bnd_Cntr_2    NUMBER := 0;
      Cursor c_v_cltb_account_insurance IS
      SELECT *
      FROM   CLTB_ACCOUNT_INSURANCE
      WHERE account_number = p_wrk_cidtkf.v_cltb_account_apps_master.account_number
      ;
   BEGIN
      Dbg('In Fn_Sys_Query..');
      IF p_QryData_Reqd = 'Q' THEN
         Dbg('Calling  Fn_Sys_Query_Desc_Fields..');
         IF NOT Fn_Sys_Query_Desc_Fields (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_Wrk_cidtkf,
            p_Err_Code  ,
            p_Err_Params ) THEN
            Dbg('Failed in Fn_Sys_Query_Desc_Fields..');
            RETURN FALSE;
         END IF;
      ELSE
         l_key := Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_APPS_MASTER.ACCOUNT_NUMBER')||'-'||
         p_cidtkf.v_cltb_account_apps_master.account_number||':'||
         Cspks_Req_Utils.Fn_Get_Item_Desc(p_source,g_ui_name,'CLTB_ACCOUNT_APPS_MASTER.BRANCH_CODE')||'-'||
         p_cidtkf.v_cltb_account_apps_master.branch_code;
         Dbg('Get The Master Record...');
         IF NVL(p_With_Lock,'N') = 'Y' THEN
            BEGIN
               SELECT *
               INTO   p_wrk_cidtkf.v_cltb_account_apps_master
               FROM  CLTB_ACCOUNT_APPS_MASTER
               WHERE account_number = p_cidtkf.v_cltb_account_apps_master.account_number
                AND branch_code = p_cidtkf.v_cltb_account_apps_master.branch_code
                FOR UPDATE NOWAIT;
            EXCEPTION
               WHEN RECORD_LOCKED THEN
                  Dbg('Failed to Obtain the Lock..');
                  Pr_Log_Error(p_Source,'ST-LOCK-001',NULL);
                  RETURN FALSE;
               WHEN No_Data_Found THEN
                  Dbg('Failed in Selecting The Master Recotrd..');
                  Dbg('Record Does not Exist..');
                  l_Rec_Exists := FALSE;
            END;

         ELSE
            BEGIN
               SELECT *
               INTO   p_Wrk_cidtkf.v_cltb_account_apps_master
               FROM  CLTB_ACCOUNT_APPS_MASTER
               WHERE account_number = p_cidtkf.v_cltb_account_apps_master.account_number
                AND branch_code = p_cidtkf.v_cltb_account_apps_master.branch_code
               ;
            EXCEPTION
               WHEN no_data_found THEN
                  Dbg('Failed in Selecting The Master Recotrd..');
                  Dbg('Record Does not Exist..');
                  p_Err_Code    := 'ST-VALS-002';
                  p_Err_Params  := l_Key;
                  RETURN FALSE;
            END;

         END IF;
         IF p_Full_Data = 'Y' AND l_Rec_Exists THEN
            Dbg('Get the Record For :CLTB_ACCOUNT_APPS_MASTER');
            BEGIN
               SELECT *
               INTO p_Wrk_cidtkf.v_cltb_account_apps_master
               FROM   CLTB_ACCOUNT_APPS_MASTER
               WHERE account_number = p_wrk_cidtkf.v_cltb_account_apps_master.account_number
                AND branch_code = p_wrk_cidtkf.v_cltb_account_apps_master.branch_code
               ;
            EXCEPTION
               WHEN OTHERS THEN
                  Dbg(SQLERRM);
                  Dbg('Failed in Selecting The Record For :CLTB_ACCOUNT_APPS_MASTER');
            END;
            Dbg('Get the Record For :CLTB_ACCOUNT_INSURANCE');
            OPEN c_v_cltb_account_insurance;
            LOOP
               FETCH c_v_cltb_account_insurance
               BULK COLLECT INTO p_Wrk_cidtkf.v_cltb_account_insurance;
                EXIT WHEN c_v_cltb_account_insurance%NOTFOUND;
            END LOOP;
            CLOSE c_v_cltb_account_insurance;

         END IF;
         IF p_QryData_Reqd = 'Y' THEN
            Dbg('Calling  Fn_Sys_Query_Desc_Fields..');
            IF NOT Fn_Sys_Query_Desc_Fields (p_Source,
               p_Source_Operation,
               p_Function_Id,
               p_Action_Code,
               p_Wrk_cidtkf,
               p_Err_Code  ,
               p_Err_Params ) THEN
               Dbg('Failed in Fn_Sys_Query_Desc_Fields..');
               RETURN FALSE;
            END IF;
         END IF;

      END IF;
      Dbg('Returning Success From Fn_Sys_Query..');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Query ..');
         Debug.Pr_Debug('**',SQLERRM);
         IF  c_v_cltb_account_insurance%ISOPEN THEN
            CLOSE c_v_cltb_account_insurance;
         END IF;
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Query;
   FUNCTION Fn_Sys_Upload_Db         (p_Source            IN VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_cidtkf     IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Prev_cidtkf     IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Wrk_cidtkf      IN OUT  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS
      l_Count             NUMBER:= 0;
      l_Ins_Count         NUMBER:= 0;
      l_Upd_Count         NUMBER:= 0;
      l_Del_Count         NUMBER:= 0;
      l_Wrk_Count         NUMBER:= 0;
      l_Prev_Count        NUMBER:= 0;
      l_Rec_Found         BOOLEAN:= FALSE;
      l_Row_Id            ROWID;
      l_Key               VARCHAR2(5000):= NULL;
      l_Auth_Stat         VARCHAR2(1) := 'A';
      l_Base_Data_From_Fc VARCHAR2(1):= 'Y';
      I_v_cltb_account_insurance       cipks_cidtkf_Main.Ty_Tb_v_cltb_account_insurance;
      U_v_cltb_account_insurance       cipks_cidtkf_Main.Ty_Tb_v_cltb_account_insurance;
      D_v_cltb_account_insurance       cipks_cidtkf_Main.Ty_Tb_v_cltb_account_insurance;
   BEGIN
      Dbg('In Fn_Sys_Upload_Db..');
      IF p_Action_Code = Cspks_Req_Global.p_new THEN

         Dbg('Inserting Into CLTB_ACCOUNT_APPS_MASTER..');
         BEGIN
            IF  p_wrk_cidtkf.v_cltb_account_apps_master.account_number IS NOT NULL AND  p_wrk_cidtkf.v_cltb_account_apps_master.branch_code IS NOT NULL THEN
               Dbg('Record Sent..');
               INSERT INTO  CLTB_ACCOUNT_APPS_MASTER
               VALUES p_wrk_cidtkf.v_cltb_account_apps_master;
            END IF;
         EXCEPTION
            WHEN OTHERS THEN
               Dbg('Failed In Insert intoCLTB_ACCOUNT_APPS_MASTER..');
               Dbg(SQLERRM);
               p_Err_Code    := 'ST-UPLD-001';
               p_Err_Params  := '@CLTB_ACCOUNT_APPS_MASTER';
               RETURN FALSE;
         END;

         Dbg('Inserting Into CLTB_ACCOUNT_INSURANCE..');
         BEGIN
            l_Count      := p_wrk_cidtkf.v_cltb_account_insurance.COUNT;
            FORALL l_index IN  1..l_count
            INSERT INTO CLTB_ACCOUNT_INSURANCE
            VALUES p_wrk_cidtkf.v_cltb_account_insurance(l_index);
         EXCEPTION
            WHEN OTHERS THEN
               Dbg('Failed In Insert intoCLTB_ACCOUNT_INSURANCE..');
               Dbg(SQLERRM);
               p_Err_Code    := 'ST-UPLD-001';
               p_Err_Params  := '@CLTB_ACCOUNT_INSURANCE';
               RETURN FALSE;
         END;
      ELSIF p_Action_Code = Cspks_Req_Global.p_modify THEN

         Dbg('Updating Single Record Node :  CLTB_ACCOUNT_APPS_MASTER..');


         Dbg('Preapring Insert and Update Types for  CLTB_ACCOUNT_INSURANCE..');
         l_Wrk_Count  := p_Wrk_cidtkf.v_cltb_account_insurance.COUNT;
         l_Prev_Count := p_Prev_cidtkf.v_cltb_account_insurance.COUNT;
         IF l_Wrk_Count > 0 THEN
            FOR l_index IN 1 .. l_Wrk_Count LOOP
               l_Rec_Found    := FALSE;
               IF l_Prev_Count >  0 THEN
                  FOR l_index1 IN 1..l_Prev_Count  LOOP
                     IF (NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index).years,-1)=  NVL(p_Prev_cidtkf.v_cltb_account_insurance  (l_index1).years,-1)) THEN
                        Dbg('Record Has Been Found.Update Case..');
                        l_rec_found := TRUE;
                        EXIT;
                     END IF;
                  END LOOP;
               END IF;
               IF l_rec_found THEN
                  Dbg('Record is Modified...');
                  U_v_cltb_account_insurance(U_v_cltb_account_insurance.COUNT +1 ) :=  p_Wrk_cidtkf.v_cltb_account_insurance(l_index);
               ELSE
                  Dbg('Record is Added...');
                  I_v_cltb_account_insurance(I_v_cltb_account_insurance.COUNT +1 ) :=  p_Wrk_cidtkf.v_cltb_account_insurance(l_index);
               END IF;
            END LOOP;
         END IF;

         Dbg('Preapring Delete Types for  CLTB_ACCOUNT_INSURANCE..');
         l_Wrk_Count  := p_wrk_cidtkf.v_cltb_account_insurance.COUNT;
         l_Prev_Count := p_prev_cidtkf.v_cltb_account_insurance.COUNT;
         IF l_Prev_Count > 0 THEN
            FOR l_index1 IN 1 .. l_Prev_Count LOOP
               l_Rec_Found    := FALSE;
               IF l_Wrk_Count >  0 THEN
                  FOR l_index IN 1..l_Wrk_Count  LOOP
                     IF (NVL(p_Wrk_cidtkf.v_cltb_account_insurance(l_index).years,-1)=  NVL(p_Prev_cidtkf.v_cltb_account_insurance  (l_index1).years,-1)) THEN
                        Dbg('Record Has Been Found.Update Case..');
                        l_Rec_Found := TRUE;
                        EXIT;
                     END IF;
                  END LOOP;
               END IF;
               IF NOT l_Rec_Found THEN
                  Dbg('Record is Deleted...');
                  D_v_cltb_account_insurance(D_v_cltb_account_insurance.COUNT +1 ) :=  p_Prev_cidtkf.v_cltb_account_insurance(l_index1);
               END IF;
            END LOOP;
         END IF;
         l_Del_Count  := D_v_cltb_account_insurance.COUNT;
         Dbg('Records Deleted  :'||l_Del_Count);
         IF l_Del_Count > 0 THEN
            FOR l_index IN 1 .. l_del_count LOOP
               Dbg('Deleting Record...');
               DELETE CLTB_ACCOUNT_INSURANCE
               WHERE account_number = D_v_cltb_account_insurance(l_index).account_number
                AND years = D_v_cltb_account_insurance(l_index).years
               ;
            END LOOP;
         END IF;
         l_Ins_Count  := I_v_cltb_account_insurance.COUNT;
         Dbg('New Records Added  :'||l_ins_count);
         BEGIN
            l_Count      := I_v_cltb_account_insurance.COUNT;
            FORALL l_Index IN  1..l_count
            INSERT INTO CLTB_ACCOUNT_INSURANCE
            VALUES I_v_cltb_account_insurance(l_index);
         EXCEPTION
            WHEN OTHERS THEN
               Dbg('Failed in Insert IntoCLTB_ACCOUNT_INSURANCE..');
               Dbg(SQLERRM);
               p_Err_Code    := 'ST-UPLD-001';
               p_Err_Params  := '@CLTB_ACCOUNT_INSURANCE';
               RETURN FALSE;
         END;
         l_Upd_Count  := U_v_cltb_account_insurance.COUNT;
         Dbg('Records Modified  :'||l_Upd_Count);
         IF l_Upd_Count > 0 THEN
            FOR l_index IN 1 .. l_Upd_Count LOOP
               Dbg('Updating The  Record...');
               BEGIN
                  UPDATE CLTB_ACCOUNT_INSURANCE
                  SET
                  EFFECTIVE_DATE = U_v_cltb_account_insurance(l_index).EFFECTIVE_DATE,
                  BASIS_AMOUNT = U_v_cltb_account_insurance(l_index).BASIS_AMOUNT,
                  DEPRECIATION_RATE = U_v_cltb_account_insurance(l_index).DEPRECIATION_RATE,
                  INSURANCE_RATE = U_v_cltb_account_insurance(l_index).INSURANCE_RATE,
                  PREMIUM_AMOUNT = U_v_cltb_account_insurance(l_index).PREMIUM_AMOUNT,
                  POLICY_NUMBER = U_v_cltb_account_insurance(l_index).POLICY_NUMBER,
                  EXPIRY_DATE = U_v_cltb_account_insurance(l_index).EXPIRY_DATE
WHERE account_number = U_v_cltb_account_insurance(l_index).account_number
 AND years = U_v_cltb_account_insurance(l_index).years
;
               EXCEPTION
                  WHEN OTHERS THEN
                     Dbg('Failed in Updating CLTB_ACCOUNT_INSURANCE..');
                     Dbg(SQLERRM);
                     p_Err_Code    := 'ST-UPLD-001';
                     p_Err_Params  := '@CLTB_ACCOUNT_INSURANCE';
                     RETURN FALSE;
               END;
            END LOOP;
         END IF;
      ELSIF p_Action_Code = Cspks_Req_Global.p_delete THEN
         Dbg('Action Code '||p_Action_Code);
         Dbg('Deleting The Data..');

         
         DELETE CLTB_ACCOUNT_INSURANCE
         WHERE account_number = p_Wrk_cidtkf.v_cltb_account_apps_master.account_number
         ;

         DELETE CLTB_ACCOUNT_APPS_MASTER WHERE account_number = p_Wrk_cidtkf.v_cltb_account_apps_master.account_number
          AND branch_code = p_Wrk_cidtkf.v_cltb_account_apps_master.branch_code
         ;



      END IF;

      Dbg('Returning Success From Fn_Sys_Upload_Db');
      RETURN TRUE;
   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Sys_Upload_Db ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_params  := NULL;
         RETURN FALSE;
   END Fn_Sys_Upload_Db;
   FUNCTION Fn_Build_Type (p_Source    IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Addl_Info       IN Cspks_Req_Global.Ty_Addl_Info,
      p_cidtkf       IN   OUT cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS

      l_Main_Function Smtb_Menu.Function_Id%TYPE := p_Function_Id;
      l_Child_Function Smtb_Menu.Function_Id%TYPE := p_Function_Id;

   BEGIN

      Dbg('In Fn_Build_Ws_Type..');

      IF Cspks_Req_Utils.Fn_Is_Req_Fc_Format(p_Source,p_Function_Id) THEN
         IF NOT Fn_Sys_Build_Fc_Type(p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_Addl_Info ,
            p_CIDTKF,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in Fn_Sys_Build_Fc_Type..');
            RETURN FALSE;
         END IF;
      ELSE
         IF NOT Fn_Sys_Build_Ws_Type(p_source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_Addl_Info ,
            p_CIDTKF,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in Fn_Sys_Build_Ws_Type..');
            RETURN FALSE;
         END IF;
      END IF;
      Pr_Skip_Handler('POSTTYPE');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Post_Build_Type_Structure(p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            l_Child_Function,
            p_Addl_Info ,
            p_CIDTKF,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in cipks_cidtkf_Custom.Fn_Post_Build_Type_Structure..');
            RETURN FALSE;
         END IF;
      END IF;
      Dbg('Returning Success From Fn_Build_Type..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of cipks_cidtkf_Main.Fn_Build_Type ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Build_Type;
   FUNCTION Fn_Build_Ts_List (p_source    IN     VARCHAR2,
                              p_source_operation  IN     VARCHAR2,
                              p_Function_id       IN     VARCHAR2,
                              p_action_code       IN     VARCHAR2,
      p_exchange_pattern   IN  VARCHAR2,
      p_cidtkf          IN cipks_cidtkf_Main.ty_cidtkf,
      p_err_code        IN OUT VARCHAR2,
      p_err_params      IN OUT VARCHAR2)
   RETURN BOOLEAN   IS

      l_Main_Function smtb_menu.function_id%TYPE := p_Function_id;

   BEGIN

      dbg('In Fn_Build_Ts_List..');

      IF Cspks_Req_Utils.Fn_Is_Res_Fc_Format(p_source,p_Function_id) THEN
         IF NOT  Fn_Sys_Build_Fc_Ts(p_Source,
            p_source_operation,
            p_Function_id,
            p_action_code,
            p_cidtkf,
            p_err_code,
            p_err_params)  THEN
            dbg('Failed in Fn_Sys_Build_Fc_Ts');
            RETURN FALSE;
         END IF;
      ELSE
         IF NOT  Fn_Sys_Build_Ws_Ts(p_Source,
            p_source_operation,
            p_Function_id,
            p_action_code,
            p_exchange_pattern,
            p_cidtkf,
            p_err_code,
            p_err_params)  THEN
            dbg('Failed in Fn_Sys_Build_Ws_Ts');
            RETURN FALSE;
         END IF;
      END IF;

      dbg('Returning from Fn_Build_Ts_List');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         debug.pr_debug('**','In when others of cipks_cidtkf_Main.Fn_Build_Ts_List ..');
         debug.pr_debug('**',SQLERRM);
         p_err_code    := 'ST-OTHR-001';
         p_err_params  := NULL;
         RETURN FALSE;
   END Fn_Build_Ts_List;
   FUNCTION Fn_Get_Key_Information (p_Source    IN  VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_cidtkf       IN  OUT cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS
      l_Key_Cols        VARCHAR2(32767);
      l_Key_Vals        VARCHAR2(32767);
      l_Func_Type       VARCHAR2(32767);
   BEGIN

      Dbg('In Fn_Get_Key_Information..');
      l_Key_Cols := 'ACCOUNT_NUMBER~'||'BRANCH_CODE~';
      l_Key_Vals := p_cidtkf.v_cltb_account_apps_master.account_number||'~'||p_cidtkf.v_cltb_account_apps_master.branch_code||'~';
      Dbg('Calling Cspks_Req_Utils.Fn_Get_Key_Information..');
      IF NOT Cspks_Req_Utils.Fn_Get_Key_Information(p_Source,
         p_Source_Operation,
         p_Function_Id,
         p_Action_Code ,
         'MAINTENANCE',
         'CLTB_ACCOUNT_APPS_MASTER',
         'BLK_ACC',
         'Acc',
         l_Key_Cols,
         l_Key_Vals,
         p_cidtkf.Addl_Info,
         p_Err_Code,
         p_Err_Params) THEN
         Dbg('Failed in  Cspks_Req_Utils.Fn_Get_Key_Information..');
         RETURN FALSE;
      END IF;
      IF p_cidtkf.Addl_Info.EXISTS('RECORD_KEY') THEN
         G_Req_Key :=  p_cidtkf.Addl_Info('RECORD_KEY');
      END IF;
      Dbg('Returning Succsess From Fn_Get_Key_Information..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of cipks_cidtkf_Main.Fn_Get_Key_Information..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Get_Key_Information;
   FUNCTION Fn_Check_Mandatory (p_Source    IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Pk_Or_Full     IN  VARCHAR2 DEFAULT 'FULL',
      p_cidtkf IN OUT  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code       IN  OUT VARCHAR2,
      p_Err_Params     IN  OUT VARCHAR2)
     RETURN BOOLEAN IS

      l_Pk_Or_Full      VARCHAR2(10) :=  'FULL';
      l_Blk      VARCHAR2(100) ;
      l_Fld      VARCHAR2(100) ;
      l_Main_Function Smtb_Menu.Function_Id%TYPE := p_Function_Id;
      l_Source_Operation      VARCHAR2(100) := p_Source_Operation;

   BEGIN

      Dbg('In Fn_Check_Mandatory..');

      IF p_Pk_Or_Full = 'FULL' OR p_Action_Code = Cspks_Req_Global.p_New THEN
         l_Pk_Or_Full := 'FULL';
      ELSE
         l_Pk_Or_Full := p_Pk_Or_Full;
      END IF;
      Pr_Skip_Handler('PREMAND');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Pre_Check_Mandatory (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_function_Id  ,
            p_cidtkf,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in  cipks_cidtkf_Custom.Fn_Pre_Check_Mandatory..');
            RETURN FALSE;
         END IF;
      END IF;

      IF NOT cipks_cidtkf_Main.Fn_Skip_Sys THEN
         Dbg('Calling   Fn_Sys_Check_Mandatory..');
         IF NOT Fn_Sys_Check_Mandatory(p_Source,
            l_Pk_Or_Full,
            p_cidtkf,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in Fn_Sys_Check_Mandatory..');
            RETURN FALSE;
         END IF;
      END IF;

      Pr_Skip_Handler('POSTMAND');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Post_Check_Mandatory (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_function_Id  ,
            l_Pk_Or_Full,
            p_cidtkf,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in  cipks_cidtkf_Custom.Fn_Post_Check_Mandatory..');
            RETURN FALSE;
         END IF;
      END IF;
      Dbg('Returning Success From Fn_Check_Mandatory..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of cipks_cidtkf_Main.Fn_Check_Mandatory ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Check_Mandatory;
   FUNCTION Fn_Query  ( p_Source    IN  VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Full_Data     IN  VARCHAR2 DEFAULT 'Y',
      p_With_Lock     IN  VARCHAR2 DEFAULT 'N',
      p_QryData_Reqd       IN  VARCHAR2,
      p_cidtkf         IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Wrk_cidtkf  IN   OUT  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS

      l_Key_Vals VARCHAR2(32767);
      l_Main_Function Smtb_Menu.Function_Id%TYPE := p_Function_Id;
      l_Source_Operation       VARCHAR2(100) := p_Source_Operation;

   BEGIN

      Dbg('In Fn_Query..');

      Pr_Skip_Handler('PREQRY');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Pre_Query (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_function_Id  ,
            p_Full_Data  ,
            p_With_Lock,
            p_QryData_Reqd,
            p_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code  ,
            p_Err_Params ) THEN
            Dbg('Failed in cipks_cidtkf_Custom.Fn_Pre_Query..');
            RETURN FALSE;
         END IF;
      END IF;
      IF NOT cipks_cidtkf_Main.Fn_Skip_Sys THEN
         IF NOT Fn_Sys_Query (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_Full_Data  ,
            p_With_Lock,
            p_QryData_Reqd,
            p_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code  ,
            p_Err_Params ) THEN
            Dbg('Failed in Fn_Sys_Query..');
            RETURN FALSE;
         END IF;
      END IF;
      Pr_Skip_Handler('POSTQRY');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Post_Query (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_function_Id  ,
            p_Full_Data  ,
            p_With_Lock,
            p_QryData_Reqd,
            p_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code  ,
            p_Err_Params ) THEN
            Dbg('Failed in cipks_cidtkf_Custom.Fn_Post_Query..');
            RETURN FALSE;
         END IF;
      END IF;
      Dbg('Returning Success From Fn_Query..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Query ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Query;
   FUNCTION Fn_Default_And_Validate        (p_Source            IN VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_cidtkf     IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Prev_cidtkf IN OUT  cipks_cidtkf_Main.Ty_cidtkf,
      p_Wrk_cidtkf IN OUT  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS
      l_Main_Function Smtb_Menu.Function_Id%TYPE := p_Function_Id;
      l_Check_Amendables  VARCHAR2(1) := 'N';
      l_Source_Operation       VARCHAR2(100) := p_Source_Operation;
      l_Full_data    VARCHAR2(1) := 'N';
      l_With_Lock    VARCHAR2(1) := 'Y';
      l_Qrydata_Reqd    VARCHAR2(1) := 'N';
      l_Pk_Or_Full      VARCHAR2(10) := 'FULL';


   BEGIN

      Dbg('In Fn_Default_And_Validate..');

      l_Full_data   := 'Y';
      Dbg('Calling  Fn_Query..');
      IF NOT Fn_Query(p_Source,
         p_Source_Operation,
         p_Function_Id,
         p_Action_Code,
         l_Full_data,
         l_With_Lock,
         l_Qrydata_Reqd,
         p_cidtkf,
         p_Prev_cidtkf,
         p_Err_Code,
         p_Err_Params) THEN
         Dbg('Failed in Fn_Query..');
         RETURN FALSE;
      END IF;
      Pr_Skip_Handler('PREDFLT');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Pre_Default_And_Validate (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_function_Id  ,
            p_cidtkf,
            p_Prev_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params) THEN
            Dbg('Failed in cipks_cidtkf_Custom.Fn_Pre_Default_And_Validate..');
            RETURN FALSE;
         END IF;
      END IF;
      IF NOT cipks_cidtkf_Main.Fn_Skip_Sys THEN
         Dbg('Calling in Fn_Sys_Default_and_Validate..');
         IF NOT Fn_Sys_Default_and_Validate (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_cidtkf,
            p_Prev_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code  ,
            p_Err_Params ) THEN
            Dbg('Failed in Fn_Sys_Default_and_Validate..');
            RETURN FALSE;
         END IF;
      END IF;
      Pr_Skip_Handler('POSTDFLT');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Post_Default_And_Validate (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_function_Id  ,
            p_cidtkf,
            p_Prev_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params) THEN
            Dbg('Failed in cipks_cidtkf_Custom.Fn_Post_Default_And_Validate..');
            RETURN FALSE;
         END IF;
      END IF;
      IF p_Action_Code = Cspks_Req_Global.p_Modify THEN
         Dbg('Calling  Fn_Check_Mandatory..');
         IF NOT Fn_Check_Mandatory(p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            l_Pk_Or_Full,
            p_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params) THEN
            Dbg('Failed in Fn_Check_Mandatory..');
            RETURN FALSE;
         END IF;
      END IF;
      Dbg('Returning Success From Fn_Default_And_Validate..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of cipks_cidtkf_Main.Fn_Default_And_Validate ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Default_And_Validate;
   FUNCTION Fn_Upload_Db  (p_Source            IN VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
      p_Post_Upl_Stat    IN     VARCHAR2,
      p_Multi_Trip_Id    IN  VARCHAR2,
      p_cidtkf     IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Prev_cidtkf     IN  cipks_cidtkf_Main.Ty_cidtkf,
      p_Wrk_cidtkf      IN OUT  cipks_cidtkf_Main.Ty_cidtkf,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS
      l_Main_Function Smtb_Menu.Function_Id%TYPE := p_Function_id;
      l_Source_Operation       VARCHAR2(100) := p_Source_Operation;
      l_Row_Id            ROWID;
      l_Key  VARCHAR2(32767);
      l_Fld  VARCHAR2(32767);


   BEGIN

      Dbg('In Fn_Upload_Db..');

      Pr_Skip_Handler('PREUPLD');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Pre_Upload_Db (p_Source,
            p_Source_operation,
            p_Function_id,
            p_Action_Code,
            p_function_Id  ,
            p_Post_Upl_Stat,
            p_Multi_Trip_Id,
            p_cidtkf,
            p_Prev_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params) THEN
            Dbg('Failed in cipks_cidtkf_Custom.Fn_Pre_Upload_Db..');
            RETURN FALSE;
         END IF;
      END IF;
      IF NOT cipks_cidtkf_Main.Fn_Skip_Sys THEN
         IF NOT Fn_Sys_Upload_Db (p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_cidtkf,
            p_Prev_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params) THEN
            Dbg('Failed in Fn_Pre_Upload_Db..');
            RETURN FALSE;
         END IF;
      END IF;

      Pr_Skip_Handler('POSTUPLD');
      IF NOT cipks_cidtkf_Main.Fn_Skip_custom  THEN
         IF NOT cipks_cidtkf_Custom.Fn_Post_Upload_Db (p_Source,
            p_Source_operation,
            p_Function_id,
            p_Action_Code,
            p_function_Id  ,
            p_Post_Upl_Stat,
            p_Multi_Trip_Id,
            p_cidtkf,
            p_Prev_cidtkf,
            p_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params) THEN
            Dbg('Failed in cipks_cidtkf_Custom.Fn_Post_Upload_Db..');
            RETURN FALSE;
         END IF;
      END IF;
      Dbg('Returning Success  From Fn_Upload_Db..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of cipks_cidtkf_Main.Fn_Upload_Db ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Upload_Db;
   FUNCTION Fn_Extract_Custom_Data (p_Source   IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
                              p_Addl_Info         IN OUT Cspks_Req_Global.Ty_Addl_Info,
                              p_Status            IN OUT VARCHAR2 ,
                              p_Err_Code          IN OUT VARCHAR2,
                              p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS

      E_Failure_Exception     EXCEPTION;
      l_cidtkf     cipks_cidtkf_Main.Ty_cidtkf;

   BEGIN

      Dbg('In Fn_Extract_Custom_Data.. ');
      IF NOT  Fn_Build_Type(p_source,
         p_Source_Operation,
         p_Function_Id,
         p_Action_Code,
         p_Addl_Info,
         l_cidtkf,
         p_Err_Code,
         p_Err_Params)  THEN
         Dbg('Failed in Fn_Build_Type..');
         p_Status      := 'F';
         RAISE e_Failure_Exception;
      END IF;
      Dbg('Calling  Fn_Get_Key_Information..');
      IF NOT  Fn_Get_Key_Information(p_Source,
         p_Source_Operation,
         p_Function_Id,
         p_Action_Code,
         l_cidtkf,
         p_Err_Code,
         p_Err_Params)  THEN
         Dbg('Failed in Fn_Get_Key_Information..');
         RAISE e_Failure_Exception;
      END IF;
      p_Addl_Info := l_cidtkf.Addl_Info;
      Dbg('Returning from Fn_Extract_Custom_Data..');
      RETURN TRUE;

   EXCEPTION
      WHEN E_Failure_Exception THEN
         Dbg('From E_Failure_Exception of Fn_Extract_Custom_Data..');
         p_Status        := 'F';
         Dbg('Errors     :'||p_Err_Code);
         Dbg('Parameters :'||p_Err_Params);
         RETURN TRUE;
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Extract_Custom_Data..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Status      := 'F';
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := Null;
         RETURN FALSE;
   END Fn_Extract_Custom_Data;

   FUNCTION Fn_Rebuild_Ts_List (p_Source    IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
                              p_Exchange_Pattern  IN     VARCHAR2,
                              p_Status            IN OUT VARCHAR2 ,
                              p_Err_Code          IN OUT VARCHAR2,
                              p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS

      E_Failure_Exception     EXCEPTION;

   BEGIN

      Dbg('In Fn_Rebuild_Ts_List ');
      IF NOT  Fn_Build_Ts_List(p_Source,
         p_Source_Operation,
         p_Function_Id,
         p_Action_Code,
         p_Exchange_Pattern,
         g_cidtkf,
         p_Err_Code,
         p_Err_Params)  THEN
         Dbg('Failed in Fn_Build_Ts_List');
         p_Status      := 'F';
         RETURN FALSE;
      END IF;
      Dbg('Returning Success From Fn_Rebuild_Ts_List..');
      RETURN TRUE;

   EXCEPTION
      WHEN E_Failure_Exception THEN
         Dbg('From E_Failure_Exception of Fn_Rebuild_Ts_List');
         p_Status        := 'F';
         Dbg('Errors     :'||p_Err_Code);
         Dbg('Parameters :'||p_Err_Params);
         RETURN TRUE;
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Rebuild_Ts_List..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Status      := 'F';
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := Null;
         RETURN FALSE;
   END Fn_Rebuild_Ts_List;

   FUNCTION Fn_Get_Node_Data (
      p_Node_Data         IN OUT Cspks_Req_Global.Ty_Tb_Chr_Node_Data,
      p_Err_Code          IN OUT VARCHAR2,
      p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN   IS
      l_Cntr NUMBER := 0;
   BEGIN

      Dbg('In Fn_Get_Node_Data..');
      l_Cntr  := Nvl(p_Node_Data.Count,0) + 1;
      p_Node_Data(l_Cntr).Node_Name := 'BLK_ACC';
      p_Node_Data(l_Cntr).Xsd_Node := 'Acc';
      p_Node_Data(l_Cntr).Node_Parent := '';
      p_Node_Data(l_Cntr).Node_Relation_Type := '1';
      p_Node_Data(l_Cntr).Query_Node := '0';
      p_Node_Data(l_Cntr).Node_Fields := 'ACC~BRN~';
      p_Node_Data(l_Cntr).Node_Tags := 'ACC~BRN~';

      l_Cntr  := Nvl(p_Node_Data.Count,0) + 1;
      p_Node_Data(l_Cntr).Node_Name := 'BLK_TKF';
      p_Node_Data(l_Cntr).Xsd_Node := 'Tkf';
      p_Node_Data(l_Cntr).Node_Parent := 'BLK_ACC';
      p_Node_Data(l_Cntr).Node_Relation_Type := 'N';
      p_Node_Data(l_Cntr).Query_Node := '0';
      p_Node_Data(l_Cntr).Node_Fields := 'YEARS~EFF_DT~BASIS_AMT~DEP_RATE~INSR_RATE~PREM_AMT~POL_NUM~EXP_DT~';
      p_Node_Data(l_Cntr).Node_Tags := 'YEARS~EFF_DT~BASIS_AMT~DEP_RATE~INSR_RATE~PREM_AMT~POL_NUM~EXP_DT~';

      Dbg('Returning From Fn_Get_Node_Data.. ');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others Of cipks_cidtkf_Main.Fn_Get_Node_Data..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := NULL;
         RETURN FALSE;
   END Fn_Get_Node_Data;
   FUNCTION Fn_Int_Main   (p_Source            IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_action_Code       IN     VARCHAR2,
                              p_Multi_Trip_Id     IN     VARCHAR2,
                              p_Request_No        IN     VARCHAR2,
                              p_cidtkf          IN OUT cipks_cidtkf_Main.ty_cidtkf,
                              p_Status            IN OUT VARCHAR2 ,
                              p_Err_Code          IN OUT VARCHAR2,
                              p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS

      E_Failure_Exception     EXCEPTION;
      E_Override_Exception    EXCEPTION;
      l_Resultant_Error_Type  VARCHAR2(32767):= 'I';
      l_Post_Upl_Stat         VARCHAR2(1) :='A';
      l_Prev_Auth_Stat        VARCHAR2(1) :='U';
      l_Wrk_cidtkf    cipks_cidtkf_Main.Ty_cidtkf;
      l_Prev_cidtkf    cipks_cidtkf_Main.Ty_cidtkf;
      l_Dmy_cidtkf    cipks_cidtkf_Main.Ty_cidtkf;
      l_Pk_Or_Full    VARCHAR2(5) :='PK';
      l_Full_Data    VARCHAR2(1) := 'Y';
      l_With_Lock    VARCHAR2(1) := 'N';
      l_Qrydata_Reqd    VARCHAR2(1) := 'Y';
      l_Count         NUMBER;
      l_Action_Code       VARCHAR2(100):= p_Action_Code;

   BEGIN

      Dbg('In Fn_Int_Main..');

      SAVEPOINT Sp_Int_Main_Cidtkf;
      p_Status := 'S';
      g_cidtkf := p_cidtkf;
      l_Wrk_cidtkf := p_cidtkf;

      Dbg('Calling  Fn_Check_Mandatory..');
      IF NOT Fn_Check_Mandatory(p_Source,
         p_Source_Operation,
         p_Function_Id,
         p_Action_Code,
         l_Pk_Or_Full,
         p_cidtkf,
         p_Err_Code,
         p_Err_Params)  THEN
         Dbg('Failed in Fn_Check_Mandatory..');
         Pr_Log_Error(p_Source,p_Err_Code, p_Err_Params);
         RAISE E_Failure_Exception;
      END IF;

      IF NOT  Fn_Get_Key_Information(p_Source,
         p_Source_Operation,
         p_Function_id,
         p_Action_Code,
         p_cidtkf,
         p_Err_Code,
         p_Err_Params)  THEN
         Dbg('Failed in Fn_Get_Key_Information..');
         RAISE e_Failure_Exception;
      END IF;
      IF p_Action_Code = Cspks_Req_Global.p_query THEN
         Dbg('Calling in Fn_Query..');
         IF NOT Fn_Query(p_Source,
            p_Source_Operation,
            p_Function_Id,
            l_Action_Code,
            l_Full_data,
            l_with_lock,
            l_Qrydata_Reqd,
            p_cidtkf,
            l_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params) THEN
            Dbg('Failed in Fn_Query..');
            Pr_Log_Error(p_Source,p_Err_Code, p_Err_Params);
            RAISE E_Failure_Exception;
         END IF;
      ELSE
         Dbg('Calling  Fn_Default_And_Validate..');
         IF NOT Fn_Default_And_Validate (p_Source,
            p_Source_Operation,
            p_Function_Id,
            l_Action_Code,
            p_cidtkf,
            l_Prev_cidtkf,
            l_Wrk_cidtkf,
            p_Err_Code,
            p_Err_Params) THEN
            Dbg('Failed in Fn_Default_And_Validate..');
            pr_log_error(p_Source,p_Err_Code, p_Err_Params);
            RAISE E_Failure_Exception;
         END IF;

         -- Get Resultant Error Type
         l_Resultant_Error_Type := Cspks_Req_Utils.Fn_Get_Resultant_Error_Type;
         IF l_Resultant_Error_Type <> 'E' THEN
            Dbg('Calling  Fn_Upload_Db..');
            IF NOT Fn_Upload_Db (p_Source,
               p_Source_Operation,
               p_Function_Id,
               l_Action_Code,
               l_Post_Upl_Stat,
               p_Multi_Trip_Id,
               p_cidtkf,
               l_Prev_cidtkf,
               l_Wrk_cidtkf,
               p_Err_Code,
               p_Err_Params) THEN
               Dbg('Failed in Fn_Upload_Db..');
               pr_log_error(p_Source,p_Err_Code, p_Err_Params);
               RAISE E_Failure_Exception;
            END IF;
            IF  l_Action_Code <> Cspks_Req_Global.p_Auth THEN
               l_Prev_Auth_Stat := 'A';
               --Get Upload Status
               Dbg('Calling Cspks_Req_Utils.Fn_Get_Auto_Auth_Status..');
               IF NOT Cspks_Req_Utils.Fn_Get_Auto_Auth_Status (p_Source,
                  p_Source_Operation,
                  p_Function_Id,
                  l_Action_Code,
                  l_Prev_Auth_Stat,
                  p_Multi_Trip_Id,
                  P_Request_No,
                  l_Post_Upl_Stat,
                  p_Err_Code,
                  p_Err_Params) THEN
                  Dbg('Failed in Cspks_Req_Utils.Fn_Get_Auto_Auth_Status..');
                  Pr_Log_Error(p_Source,p_Err_Code,p_Err_Params);
                  RAISE E_Failure_Exception;
               END IF;

               IF l_Post_Upl_Stat NOT IN ('A','U','O') THEN
                  Dbg('Cannot Proceed Further..');
                  RAISE E_Failure_Exception;
               ELSE
                  IF l_post_upl_stat = 'A'THEN
                     Dbg('Calling  Fn_Upload_Db..');
                     IF NOT Fn_Upload_Db (p_Source,
                        p_Source_Operation,
                        p_Function_Id,
                        Cspks_Req_Global.p_auth,
                        l_Post_Upl_Stat,
                        p_Multi_Trip_Id,
                        p_cidtkf,
                        l_Prev_cidtkf,
                        l_Wrk_cidtkf,
                        p_Err_Code,
                        p_Err_Params) THEN
                        Dbg('Failed in Fn_Upload_Db..');
                        Pr_Log_Error(p_Source,p_Err_Code, p_Err_Params);
                        RAISE E_Failure_Exception;
                     END IF;
                  END IF;
               END IF;
            END IF;
            --Get Upload Status
            Dbg('Calling  Cspks_Req_Utils.Fn_Get_Upload_Status..');
            IF NOT Cspks_Req_Utils.Fn_Get_Upload_Status (p_Source,
               p_source_operation,
               p_Function_Id,
               l_Action_Code,
               p_Multi_Trip_Id,
               P_Request_No,
               l_Post_Upl_Stat,
               p_Err_Code,
               p_Err_Params) THEN
               Dbg('Failed in Cspks_Req_Utils.Fn_Get_Upload_Status..');
               Pr_Log_Error(p_Source,p_Err_Code,p_Err_Params);
               RAISE E_Failure_Exception;
            END IF;

            IF l_Post_Upl_Stat IN ('A','U') THEN
               Dbg('Success Case...');
               IF l_Action_Code <> Cspks_Req_Global.p_Delete THEN
                  IF NOT Fn_Query(p_Source,
                     p_Source_Operation,
                     p_Function_Id,
                     l_Action_Code,
                     l_Full_Data,
                     l_With_Lock,
                     l_Qrydata_Reqd,
                     l_Wrk_cidtkf,
                     l_Wrk_cidtkf,
                     p_Err_Code,
                     p_Err_Params) THEN
                     Dbg('Failed in Fn_Query..');
                     Pr_Log_Error(p_Source,p_Err_Code, p_Err_Params);
                     RAISE E_Failure_Exception;
                  END IF;
               END IF;
            ELSIF l_Post_Upl_Stat ='O' THEN
               Dbg('Raising Override Exception..');
               RAISE E_Override_Exception;
            ELSE
               Dbg('Not Feasible to Proceed..');
               RAISE E_Failure_Exception;
            END IF;
         ELSE
            Dbg('Encountered Errros..');
            RAISE E_Failure_Exception;
         END IF;
      END IF; -- Action Code

      Cspks_Req_Utils.Pr_Get_Final_Err_Code(p_Function_Id,l_Action_Code,l_Post_Upl_Stat,p_Err_Code,p_Err_Params);
      Pr_Log_Error(p_Source,p_Err_Code,p_Err_Params);
      g_cidtkf := l_wrk_cidtkf;
      IF l_Action_Code = Cspks_Req_Global.p_Delete AND p_Source = 'FLEXCUBE'  THEN
         l_Wrk_cidtkf := l_Dmy_cidtkf;
      END IF;
      p_cidtkf := l_wrk_cidtkf;
      Dbg('Errors     :'||p_Err_Code);
      Dbg('Parameters :'||p_Err_Params);

      Dbg('Returning Success From Fn_Int_Main..');
      RETURN TRUE;

   EXCEPTION
      WHEN E_Failure_Exception THEN
         Dbg('From E_Failure_Exception of Fn_Int_Main..');
         ROLLBACK TO Sp_Int_Main_Cidtkf;
         p_Status        := 'F';
         l_Post_Upl_Stat := 'F';
         Cspks_Req_Utils.Pr_Get_Final_Err_Code(p_Function_Id,l_Action_Code,l_Post_Upl_Stat,p_Err_Code,p_Err_Params);
         Pr_Log_Error(p_Source,p_Err_Code,p_Err_Params);
         Dbg('Errors     :'||p_Err_Code);
         Dbg('Parameters :'||p_Err_Params);
         RETURN TRUE;

      WHEN E_Override_Exception THEN
         Dbg('From E_Override_Exception of Fn_Int_Main');
         p_Status        := 'O';
         l_post_upl_stat := 'O';
         IF NOT Cspks_Req_Utils.Fn_Log_Overrides(p_Multi_Trip_Id, p_Request_No, p_Err_Code, p_Err_Params) THEN
            Dbg('Failed inCspks_Req_Utils.Fn_Log_Overrides..');
            p_Err_Code    := 'ST-OTHR-001';
            p_Err_Params  := Null;
            RETURN FALSE;
         END IF;
         Cspks_Req_Utils.Pr_Get_Final_Err_Code(p_Function_Id,l_Action_Code,l_Post_Upl_Stat,p_Err_Code,p_Err_Params);
         Pr_Log_Error(p_Source,p_Err_Code,p_Err_Params);
         Dbg('Errors     :'||p_Err_Code);
         Dbg('Parameters :'||p_Err_Params);
         RETURN TRUE;

      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Int_Main ..');
         Debug.Pr_Debug('**',SQLERRM);
         ROLLBACK TO Sp_Int_Main_Cidtkf;
         p_Status      := 'F';
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := Null;
         RETURN FALSE;
   END Fn_Int_Main;

   FUNCTION Fn_Main   (p_Source            IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
                              p_Multi_Trip_Id     IN     VARCHAR2,
                              p_Request_No        IN     VARCHAR2,
                              p_cidtkf          IN OUT cipks_cidtkf_Main.ty_cidtkf,
                              p_Status            IN OUT VARCHAR2 ,
                              p_Err_Code          IN OUT VARCHAR2,
                              p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS

      E_Failure_Exception     EXCEPTION;
      E_Override_Exception    EXCEPTION;

   BEGIN

      Dbg('In Fn_Main..');
      SAVEPOINT Sp_Main_Cidtkf;
      Dbg('Calling  Fn_Int_Main..');
      IF NOT  Fn_Int_Main(p_Source,
         p_Source_Operation,
         p_Function_id,
         p_Action_Code,
         p_Multi_Trip_Id,
         p_Request_No,
         p_cidtkf,
         p_Status,
         p_Err_Code,
         p_Err_Params)  THEN
         Dbg('Failed in Fn_Int_Main..');
         RAISE E_Failure_Exception;
      END IF;
      IF p_Status = 'F' THEN
         RAISE E_Failure_Exception;
      ELSIF p_Status = 'O' THEN
         RAISE E_Override_Exception;
      END IF;
      Dbg('Returning Success From Fn_Main..');
      RETURN TRUE;

   EXCEPTION
      WHEN E_Failure_Exception THEN
         Dbg('From E_Failure_Exception of Fn_Main');
         ROLLBACK TO Sp_Main_Cidtkf;
         p_Status      := 'F';
         RETURN TRUE;

      WHEN E_Override_Exception THEN
         Dbg('From E_Override_Exception of Fn_Main..');
         p_Status      := 'O';
         RETURN TRUE;

      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Main ..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Status      := 'F';
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := Null;
         ROLLBACK TO Sp_Main_Cidtkf;
         RETURN FALSE;
   END Fn_Main;

   FUNCTION Fn_Process_Request (p_Source    IN     VARCHAR2,
                              p_Source_Operation  IN     VARCHAR2,
                              p_Function_Id       IN     VARCHAR2,
                              p_Action_Code       IN     VARCHAR2,
                              p_Exchange_Pattern  IN     VARCHAR2,
                              p_Multi_Trip_Id     IN     VARCHAR2,
                              p_Request_No        IN     VARCHAR2,
                              p_Addl_Info         IN OUT Cspks_Req_Global.Ty_Addl_Info,
                              p_Status            IN OUT VARCHAR2 ,
                              p_Err_Code          IN OUT VARCHAR2,
                              p_Err_Params        IN OUT VARCHAR2)
   RETURN BOOLEAN IS

      l_cidtkf     cipks_cidtkf_Main.ty_cidtkf;

   BEGIN

      Dbg('In Fn_Process_Request ');
      IF NOT  Fn_Build_Type(p_Source,
         p_Source_Operation,
         p_Function_id,
         p_Action_Code,
         p_Addl_Info,
         l_cidtkf,
         p_Err_Code,
         p_Err_Params)  THEN
         Dbg('Failed in Fn_Build_Type..');
         p_status      := 'F';
         RETURN FALSE;
      END IF;
      IF Cspks_Req_Global.Fn_UnTanking THEN
         IF NOT  Fn_Int_Main(p_Source,
            p_Source_Operation,
            p_Function_Id,
            p_Action_Code,
            p_Multi_Trip_Id,
            p_Request_No,
            l_cidtkf,
            p_Status,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in Fn_main..');
            RETURN FALSE;
         END IF;
      ELSE
         IF NOT  Fn_Main(p_source,
            p_Source_Operation,
            p_Function_id,
            p_Action_Code,
            p_Multi_Trip_Id,
            p_Request_No,
            l_cidtkf,
            p_Status,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in Fn_main..');
            RETURN FALSE;
         END IF;
      END IF;

      p_addl_info := l_cidtkf.Addl_Info;
      IF Cspks_Req_Global.Fn_Build_Resp THEN
         Cspks_Req_Global.Pr_Reset;
         IF NOT  Fn_Build_Ts_List(p_Source,
            p_Source_Operation,
            p_Function_id,
            p_Action_Code,
            p_Exchange_Pattern,
            l_cidtkf,
            p_Err_Code,
            p_Err_Params)  THEN
            Dbg('Failed in Fn_Build_Ts_List..');
            p_Status      := 'F';
            RETURN FALSE;
         END IF;
         Cspks_Req_Global.Pr_Close_Ts;
      END IF;
      Dbg('Returning Success From Fn_Process_Request..');
      RETURN TRUE;

   EXCEPTION
      WHEN OTHERS THEN
         Debug.Pr_Debug('**','In When Others of Fn_Process_Request..');
         Debug.Pr_Debug('**',SQLERRM);
         p_Status      := 'F';
         p_Err_Code    := 'ST-OTHR-001';
         p_Err_Params  := Null;
         RETURN FALSE;
   END Fn_Process_Request;

END cipks_cidtkf_main;
/