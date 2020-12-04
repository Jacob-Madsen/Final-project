/* This Proc Format program was automatically generated by Stat/Transfer ver.14.2.1138.0513 */



libname library '/home/Carsten/Skrivebord/' ;

proc format library = library ;
   value V3F
      829 = '1981 undersøgelsen'  
      1523 = '1990 undersøgelsen'  
      5356 = '1999 undersøgelsen' ;
   value V5F
      1981 = '1981'  
      1990 = '1990'  
      1999 = '1999' ;
   value V6F
      1 = 'Mand'  
      2 = 'Kvinde' ;
   value V8F
      1 = 'Gift'  
      2 = 'Samlevende i papirløst ægteskab'  
      3 = 'Skilt'  
      4 = 'Separeret'  
      5 = 'Enke/enkemand'  
      6 = 'Enlig' ;
   value V9F
      1 = 'Ugift'  
      2 = 'Gift'  
      4 = 'Skilt'  
      5 = 'Enke/enkemand'  
      6 = 'Reg. partner'  
      10 = 'Irrelevant'  
      11 = 'Deltog ikke' ;
   value V10F
      1 = 'Ja'  
      2 = 'Nej'  
      9 = 'Uoplyst' ;
   value V11F
      1 = '1 barn'  
      2 = '2 børn'  
      3 = '3 børn'  
      4 = '4 børn'  
      5 = '5 børn'  
      6 = '6 børn eller flere'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V12F
      1 = '12 år eller yngre'  
      2 = '13 år'  
      3 = '14 år'  
      4 = '15 år'  
      5 = '16 år'  
      6 = '17 år'  
      7 = '18 år'  
      8 = '19 år'  
      9 = '20 år'  
      10 = '21 år eller mere'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V13F
      1 = 'Ja, lønmodtager 30 timer om ugen eller mere, gå til spm. 21 (ikke medtaget i datamaterialet)'  
      2 = 'Ja, lønmodtager mindre end 30 timer om ugen, gå til spm. 21 (Ikke medtaget i datamaterialet)'  
      3 = 'Ja, selvstændig, gå til spm. 24 (Ikke medtaget i datamaterialet)'  
      4 = 'Nej, efterlønner/førtidspensionist/ pensionist, gå til spm. 25 (Ikke medtaget i datamaterielet)'  
      5 = 'Nej, hjemmegående, gå til spm. 29 (V15)'  
      6 = 'Nej, studerende, gå til spm. 29 (V15)'  
      7 = 'Nej, arbejdsløs'  
      8 = 'Nej, andet, hvilket:, gå til spm. 29 (V15)'  
      99 = 'Uoplyst' ;
   value V14F
      1 = 'Selvstændig landmand'  
      2 = 'Selvstændig akademiker, evt. med 1 eller 2 underordnede (advokat, ingeniør)'  
      3 = 'Medhjælpende ægtefælle'  
      4 = 'Ejer/direktør for virksomheden (hvis ikke kode 02) med 10 eller flere ansatte'  
      5 = 'Ejer/direktør for virksomheden (hvis ikke kode 02) med færre end 10 ansatte'  
      6 = 'Arbejder, faglært'  
      7 = 'Specialarbejder (dvs. ufaglært med efteruddannelseskurser)'  
      8 = 'Ufaglært arbejder'  
      9 = 'Formand, inspektør, funktionær med ledelsesopgaver'  
      10 = 'Funktionær med faguddannelse'  
      11 = 'Funktionær uden faguddannelse'  
      12 = 'Landarbejder'  
      13 = 'Ansat ved forsvaret'  
      14 = 'Lærling, EFG-elev i håndværksfag'  
      15 = 'Lærling, EFG-elev i butiks- eller kontorfag'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst'  
      100 = 'Irrelevant' ;
   value V15F
      1 = 'Ja, gå til spm. 34 (V17)'  
      2 = 'Nej'  
      9 = 'Uoplyst' ;
   value V16F
      1 = 'Selvstændig landmand'  
      2 = 'Selvstændig akademiker, evt. med 1 eller 2 underordnede (advokat, ingeniør)'  
      3 = 'Medhjælpende ægtefælle'  
      4 = 'Ejer/direktør for virksomheden (hvis ikke kode 02) med 10 eller flere ansatte'  
      5 = 'Ejer/direktør for virksomheden (hvis ikke kode 02) med færre end 10 ansatte'  
      6 = 'Arbejder, faglært'  
      7 = 'Specialarbejder (dvs. ufaglært med efteruddannelseskurser)'  
      8 = 'Ufaglært arbejder'  
      9 = 'Formand, inspektør, funktionær med ledelsesopgaver'  
      10 = 'Funktionær med faguddannelse'  
      11 = 'Funktionær uden faguddannelse'  
      12 = 'Landarbejder'  
      13 = 'Ansat ved forsvaret'  
      14 = 'Lærling, EFG-elev i håndværksfag'  
      15 = 'Lærling, EFG-elev i butiks- eller kontorfag'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst'  
      100 = 'Irrelevant' ;
   value V17F
      1 = 'Mindre end 60.000 kr.'  
      2 = '60.000 - 79.999 kr.'  
      3 = '80.000 - 99.999 kr.'  
      4 = '100.000 - 124.999 kr.'  
      5 = '125.000 - 149.999 kr.'  
      6 = '150.000 - 199.999 kr.'  
      7 = '200.000 - 249.999 kr.'  
      8 = '250.000 kr. eller mere'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V18F
      1 = 'Ofte'  
      2 = 'Nu og da'  
      3 = 'Aldrig'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V19F
      1 = 'Meget lykkelig'  
      2 = 'Ret lykkelig'  
      3 = 'Ikke særlig lykkelig'  
      4 = 'Meget ulykkelig'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V20F
      1 = 'Tilhører ingen af de nævnte foreninger, gå til spm. 41 (Ej medtaget i datamaterialet)'  
      2 = 'Tilhører en eller flere af de nævnte foreninger'  
      9 = 'Uoplyst' ;
   value V21F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V22F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V23F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V24F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V25F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V26F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V27F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V28F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V29F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V30F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V31F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V32F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V33F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V34F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V35F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V36F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V37F
      1 = 'Tilhører'  
      2 = 'Tilhører ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V38F
      1 = 'Ja'  
      2 = 'Nej'  
      10 = 'Irrelevant' ;
   value V39F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V40F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V41F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V42F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V43F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V44F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V45F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V46F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V47F
      1 = 'De fleste er til at stole på'  
      2 = 'Man kan ikke være for forsigtig'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V48F
      1 = 'Overhovedet ingen indflydelse'  
      10 = 'Overordentlig megen indflydelse'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V49F
      1 = 'Meget utilfreds'  
      10 = 'Meget tilfreds'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V50F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V51F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V52F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V53F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V54F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V55F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V56F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V57F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V58F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V59F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V60F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V61F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V62F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V63F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V64F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V65F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V66F
      1 = 'Meget'  
      2 = 'Noget'  
      3 = 'Lidt'  
      4 = 'Slet ikke'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V67F
      1 = 'Meget utilfreds'  
      10 = 'Meget tilfreds'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst'  
      100 = 'Irrelevant' ;
   value V68F
      1 = 'Slet ingen frihed'  
      10 = 'Meget stor frihed'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst'  
      100 = 'Irrelevant' ;
   value V69F
      1 = 'Retfærdigt'  
      2 = 'Uretfærdigt'  
      3 = 'Ved ikke'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V70F
      1 = 'Man bør følge arbejdsinstruktionerne'  
      2 = 'Man må først være overbevist om, at disse er rigtige'  
      3 = 'Det kommer an på den konkrete situation'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V71F
      1 = 'Mest enig i udsagn A'  
      2 = 'Mest enig i udsagn B'  
      3 = 'Hverken enig i udsagn A eller B'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V72F
      1 = 'Ja'  
      2 = 'Nej, gå til spm. 59 (Ikke medtaget i datamaterialet)'  
      9 = 'Uoplyst' ;
   value V73F
      1 = 'Romersk katolsk'  
      2 = 'Den danske Folkekirke'  
      3 = 'Jehovas Vidne, adventist mv.'  
      4 = 'Jøde'  
      5 = 'Muslim'  
      6 = 'Hindu'  
      7 = 'Buddhist'  
      8 = 'Andet, skriv hvilken'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst'  
      100 = 'Irrelevant' ;
   value V74F
      1 = 'Oftere end 1 gang om ugen'  
      2 = 'Ca. 1 gang om ugen'  
      3 = 'Ca. 1 gang om måneden'  
      4 = 'Kun til jule-påskegudstjeneste'  
      5 = 'Til andre højtider'  
      6 = 'Ca. 1 gang om året'  
      7 = 'Sjældnere end 1 gang om året'  
      8 = 'Aldrig, næsten aldrig'  
      9 = 'Vil ikke svare'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V75F
      1 = 'Et troende menneske'  
      2 = 'Et ikke troende menneske'  
      3 = 'Overbevist ateist'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V76F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V77F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V78F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V79F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V80F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V81F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V82F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V83F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V84F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V85F
      1 = 'Der er en personlig Gud'  
      2 = 'Der er en særlig åndelig kraft'  
      3 = 'Jeg ved ikke, hvad jeg skal tro'  
      4 = 'Jeg tror ikke, der er nogen form for åndelig kraft eller personlig Gud'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V86F
      1 = 'Slet ingen rolle'  
      10 = 'Meget stor rolle'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V87F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V88F
      1 = 'Ja'  
      2 = 'Nej'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V89F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V90F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V91F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V92F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V93F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V94F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V95F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V96F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V97F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V98F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V99F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V100F
      1 = 'Meget vigtigt'  
      2 = 'Ret vigtigt'  
      3 = 'Ikke særlig vigtigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V101F
      1 = 'Enig'  
      2 = 'Uenig'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V102F
      1 = 'Væsentligt at få børn'  
      2 = 'Ikke væsentligt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V103F
      1 = 'Enig'  
      2 = 'Uenig'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V104F
      1 = 'Ja'  
      2 = 'Nej'  
      3 = 'Det kommer an på'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V105F
      1 = 'Mest enig i udsagn A'  
      2 = 'Mest enig i udsagn B'  
      3 = 'Ingen af delene'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V106F
      1 = 'Det er forældrenes pligt at gøre alt, hvad der står i deres magt for deres børn, uanset om det skulle gå ud over de'  
      2 = 'Forældre har også deres eget liv, og man kan ikke forlange, at de skal ofre deres egen trivsel for børnenes skyld'  
      3 = 'Ingen af delene'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V107F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V108F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V109F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V110F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V111F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V112F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V113F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V114F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V115F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V116F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V117F
      0 = 'Ikke nævnt'  
      1 = 'Nævnt'  
      9 = 'Uoplyst' ;
   value V118F
      1 = 'Tilhænger'  
      2 = 'Modstander'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V119F
      1 = 'Tilhænger'  
      2 = 'Modstander'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V120F
      1 = 'Har deltaget'  
      2 = 'Ville muligvis deltage'  
      3 = 'Ville aldrig deltage'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V121F
      1 = 'Har deltaget'  
      2 = 'Ville muligvis deltage'  
      3 = 'Ville aldrig deltage'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V122F
      1 = 'Har deltaget'  
      2 = 'Ville muligvis deltage'  
      3 = 'Ville aldrig deltage'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V123F
      1 = 'Har deltaget'  
      2 = 'Ville muligvis deltage'  
      3 = 'Ville aldrig deltage'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V124F
      1 = 'Har deltaget'  
      2 = 'Ville muligvis deltage'  
      3 = 'Ville aldrig deltage'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V125F
      1 = 'Enig i udsagn A'  
      2 = 'Enig i udsagn B'  
      3 = 'Hverken enig i udsagn A eller B'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V126F
      1 = 'Venstreorienteret'  
      10 = 'Højreorienteret'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V127F
      1 = 'At oprette lov og orden i landet'  
      2 = 'At lade folk bestemme mere i vigtige regeringsbeslutninger'  
      3 = 'At bekæmpe stigende priser'  
      4 = 'At beskytte ytringsfriheden'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V128F
      1 = 'At oprette lov og orden i landet'  
      2 = 'At lade folk bestemme mere i vigtige regeringsbeslutninger'  
      3 = 'At bekæmpe stigende priser'  
      4 = 'At beskytte ytringsfriheden'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V129F
      1 = 'Godt'  
      2 = 'Dårligt'  
      3 = 'Ligegyldigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V130F
      1 = 'Godt'  
      2 = 'Dårligt'  
      3 = 'Ligegyldigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V131F
      1 = 'Godt'  
      2 = 'Dårligt'  
      3 = 'Ligegyldigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V132F
      1 = 'Godt'  
      2 = 'Dårligt'  
      3 = 'Ligegyldigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V133F
      1 = 'Godt'  
      2 = 'Dårligt'  
      3 = 'Ligegyldigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V134F
      1 = 'Godt'  
      2 = 'Dårligt'  
      3 = 'Ligegyldigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V135F
      1 = 'Godt'  
      2 = 'Dårligt'  
      3 = 'Ligegyldigt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V136F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V137F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V138F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V139F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V140F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V141F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V142F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V143F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V144F
      1 = 'Meget stor tillid'  
      2 = 'Ret stor tillid'  
      3 = 'Ikke særlig stor tillid'  
      4 = 'Slet ingen tillid'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V145F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V146F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V147F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V148F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V149F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V150F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V151F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V152F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V153F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V154F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst'  
      100 = 'Irrelevant'  
      101 = 'Deltog ikke (spørgsmål ikke stillet i 1990)' ;
   value V155F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V156F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V157F
      1 = 'Slet ikke'  
      10 = 'I høj grad'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst' ;
   value V158F
      1 = 'Den by eller det lokalområde jeg bor i'  
      2 = 'Den egn eller den landsdel jeg bor i'  
      3 = 'Mit land, hvilket:'  
      4 = 'Europa'  
      5 = 'Verden'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V159F
      1 = 'Den by eller det lokalområde jeg bor i'  
      2 = 'Den egn eller den landsdel jeg bor i'  
      3 = 'Mit land, hvilket:'  
      4 = 'Europa'  
      5 = 'Verden'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V160F
      1 = 'Meget stolt'  
      2 = 'Ret stolt'  
      3 = 'Ikke særlig stolt'  
      4 = 'Slet ikke stolt'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst'  
      10 = 'Irrelevant' ;
   value V161F
      1 = 'Socialdemokratiet'  
      2 = 'Det Radikale Venstre'  
      3 = 'Det Konservative Folkeparti'  
      4 = 'Centrum-Demokraterne'  
      5 = 'Socialistisk Folkeparti'  
      6 = 'Dansk Folkeparti'  
      7 = 'Kristeligt Folkeparti'  
      8 = 'Venstre, Danmarks Liberale Parti'  
      9 = 'Fremskridtspartiet'  
      10 = 'Enhedslisten - De rød-grønne'  
      11 = 'De Grønne'  
      13 = 'Internationalen-Socialistisk Arbejderparti'  
      14 = 'Danmarks Kommunistiske Parti'  
      16 = 'Fælles Kurs'  
      17 = 'Venstresocialisterne'  
      19 = 'Danmarks Retsforbund'  
      20 = 'Ingen af partierne'  
      21 = 'Har ikke stemmeret'  
      22 = 'Vil ikke stemme'  
      23 = 'Vil stemme blankt'  
      24 = 'Vil ikke svare'  
      88 = 'Ved ikke'  
      99 = 'Uoplyst'  
      100 = 'Irrelevant' ;
   value V162F
      1 = 'Meget god'  
      2 = 'God'  
      3 = 'Nogenlunde'  
      4 = 'Dårlig'  
      5 = 'Meget dårlig'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V163F
      1 = 'Ofte'  
      2 = 'En gang imellem'  
      3 = 'Sjældent'  
      4 = 'Aldrig'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V164F
      1 = 'Københavns Amt'  
      2 = 'Roskilde Amt'  
      3 = 'Frederiksborg Amt'  
      4 = 'Vestsjællands Amt'  
      5 = 'Storstrøms Amt'  
      6 = 'Fyns Amt'  
      7 = 'Nordjyllands Amt'  
      8 = 'Viborg Amt'  
      9 = 'Ringkøbing Amt'  
      10 = 'Århus Amt'  
      11 = 'Vejle Amt'  
      12 = 'Ribe/Sønderjyllands Amt'  
      14 = 'Bornholms Amt'  
      15 = 'Københavns og Frederiksberg Kommune (Oprindeligt kodet 99)' ;
   value V165F
      1 = 'Hovedstad'  
      2 = 'Øerne i øvrigt'  
      3 = 'Jylland' ;
   value LABA
      1 = '18-29 år'  
      2 = '30-39 år'  
      3 = '40-49 år'  
      4 = '50-59 år'  
      5 = '60-69 år'  
      6 = '70-79 år'  
      7 = '80-89 år'  
      8 = '90-99 år' ;
   value V170F
      1 = '18-35 år'  
      2 = '36-55 år'  
      3 = 'Ældre end 56 år' ;
   value V171F
      1 = 'Mellem 18 og 25 år'  
      2 = 'Mellem 26 og 95 år' ;
   value V172F
      1 = 'Født 1973-1981'  
      2 = 'Født 1964-1972'  
      3 = 'Født 1955-1963'  
      4 = 'Født 1946-1954'  
      5 = 'Født 1937-1945'  
      6 = 'Født 1928-1936'  
      7 = 'Født 1919-1927'  
      8 = 'Født 1910-1918'  
      9 = 'Født 1901-1909'  
      10 = 'Født 1892-1900'  
      11 = 'Født 1883-1891' ;
   value V173F
      1 = 'Født 1972-1981'  
      2 = 'Født 1962-1971'  
      3 = 'Født 1952-1961'  
      4 = 'Født 1942-1951'  
      5 = 'Født 1932-1941'  
      6 = 'Født 1922-1931'  
      7 = 'Født 1912-1921'  
      8 = 'Født 1902-1911'  
      9 = 'Født 1892-1901'  
      10 = 'Født 1882-1891' ;
   value V174F
      1 = 'Under 150.000 kr'  
      2 = 'Mellem 151.000 og 250.000 kr'  
      3 = 'Mere end 250.000 kr'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V175F
      1 = 'Går ind for kernefamilien'  
      2 = 'Både- og'  
      3 = 'Går ind for nye familieværdier'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V176F
      1 = 'Materialister'  
      2 = 'Både - og'  
      3 = 'Post-materialister'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V177F
      1 = 'Aktiv, dansk statsborger'  
      10 = 'Irrelevant' ;
   value V178F
      100 = 'Irrelevant' ;
   value V179F
      4 = 'April'  
      5 = 'Maj'  
      6 = 'Juni'  
      9 = 'September'  
      10 = 'Oktober'  
      11 = 'November'  
      100 = 'Irrelevant' ;
   value V180F
      99 = 'Uoplyst'  
      100 = 'Irrelevant' ;
   value V183F
      1 = 'Forår/sommer'  
      2 = 'Sensommer/efterår'  
      10 = 'Irrelevant' ;
   value V184F
      1 = 'Mindst en gang om måneden'  
      2 = 'Til højtider'  
      3 = 'Højst en gang om året'  
      4 = 'Aldrig eller næsten aldrig'  
      5 = 'Vil ikke svare (Oprindeligt kodet 9)'  
      8 = 'Ved ikke'  
      9 = 'Uoplyst' ;
   value V185F
      1 = 'Lav'  
      2 = 'Høj' ;

proc datasets library = library ;
modify data9990;
   format        V3 V3F.;
   format        V5 V5F.;
   format        V6 V6F.;
   format        V8 V8F.;
   format        V9 V9F.;
   format       V10 V10F.;
   format       V11 V11F.;
   format       V12 V12F.;
   format       V13 V13F.;
   format       V14 V14F.;
   format       V15 V15F.;
   format       V16 V16F.;
   format       V17 V17F.;
   format       V18 V18F.;
   format       V19 V19F.;
   format       V20 V20F.;
   format       V21 V21F.;
   format       V22 V22F.;
   format       V23 V23F.;
   format       V24 V24F.;
   format       V25 V25F.;
   format       V26 V26F.;
   format       V27 V27F.;
   format       V28 V28F.;
   format       V29 V29F.;
   format       V30 V30F.;
   format       V31 V31F.;
   format       V32 V32F.;
   format       V33 V33F.;
   format       V34 V34F.;
   format       V35 V35F.;
   format       V36 V36F.;
   format       V37 V37F.;
   format       V38 V38F.;
   format       V39 V39F.;
   format       V40 V40F.;
   format       V41 V41F.;
   format       V42 V42F.;
   format       V43 V43F.;
   format       V44 V44F.;
   format       V45 V45F.;
   format       V46 V46F.;
   format       V47 V47F.;
   format       V48 V48F.;
   format       V49 V49F.;
   format       V50 V50F.;
   format       V51 V51F.;
   format       V52 V52F.;
   format       V53 V53F.;
   format       V54 V54F.;
   format       V55 V55F.;
   format       V56 V56F.;
   format       V57 V57F.;
   format       V58 V58F.;
   format       V59 V59F.;
   format       V60 V60F.;
   format       V61 V61F.;
   format       V62 V62F.;
   format       V63 V63F.;
   format       V64 V64F.;
   format       V65 V65F.;
   format       V66 V66F.;
   format       V67 V67F.;
   format       V68 V68F.;
   format       V69 V69F.;
   format       V70 V70F.;
   format       V71 V71F.;
   format       V72 V72F.;
   format       V73 V73F.;
   format       V74 V74F.;
   format       V75 V75F.;
   format       V76 V76F.;
   format       V77 V77F.;
   format       V78 V78F.;
   format       V79 V79F.;
   format       V80 V80F.;
   format       V81 V81F.;
   format       V82 V82F.;
   format       V83 V83F.;
   format       V84 V84F.;
   format       V85 V85F.;
   format       V86 V86F.;
   format       V87 V87F.;
   format       V88 V88F.;
   format       V89 V89F.;
   format       V90 V90F.;
   format       V91 V91F.;
   format       V92 V92F.;
   format       V93 V93F.;
   format       V94 V94F.;
   format       V95 V95F.;
   format       V96 V96F.;
   format       V97 V97F.;
   format       V98 V98F.;
   format       V99 V99F.;
   format      V100 V100F.;
   format      V101 V101F.;
   format      V102 V102F.;
   format      V103 V103F.;
   format      V104 V104F.;
   format      V105 V105F.;
   format      V106 V106F.;
   format      V107 V107F.;
   format      V108 V108F.;
   format      V109 V109F.;
   format      V110 V110F.;
   format      V111 V111F.;
   format      V112 V112F.;
   format      V113 V113F.;
   format      V114 V114F.;
   format      V115 V115F.;
   format      V116 V116F.;
   format      V117 V117F.;
   format      V118 V118F.;
   format      V119 V119F.;
   format      V120 V120F.;
   format      V121 V121F.;
   format      V122 V122F.;
   format      V123 V123F.;
   format      V124 V124F.;
   format      V125 V125F.;
   format      V126 V126F.;
   format      V127 V127F.;
   format      V128 V128F.;
   format      V129 V129F.;
   format      V130 V130F.;
   format      V131 V131F.;
   format      V132 V132F.;
   format      V133 V133F.;
   format      V134 V134F.;
   format      V135 V135F.;
   format      V136 V136F.;
   format      V137 V137F.;
   format      V138 V138F.;
   format      V139 V139F.;
   format      V140 V140F.;
   format      V141 V141F.;
   format      V142 V142F.;
   format      V143 V143F.;
   format      V144 V144F.;
   format      V145 V145F.;
   format      V146 V146F.;
   format      V147 V147F.;
   format      V148 V148F.;
   format      V149 V149F.;
   format      V150 V150F.;
   format      V151 V151F.;
   format      V152 V152F.;
   format      V153 V153F.;
   format      V154 V154F.;
   format      V155 V155F.;
   format      V156 V156F.;
   format      V157 V157F.;
   format      V158 V158F.;
   format      V159 V159F.;
   format      V160 V160F.;
   format      V161 V161F.;
   format      V162 V162F.;
   format      V163 V163F.;
   format      V164 V164F.;
   format      V165 V165F.;
   format      V167 LABA.;
   format      V169 LABA.;
   format      V170 V170F.;
   format      V171 V171F.;
   format      V172 V172F.;
   format      V173 V173F.;
   format      V174 V174F.;
   format      V175 V175F.;
   format      V176 V176F.;
   format      V177 V177F.;
   format      V178 V178F.;
   format      V179 V179F.;
   format      V180 V180F.;
   format      V183 V183F.;
   format      V184 V184F.;
   format      V185 V185F.;
quit;
