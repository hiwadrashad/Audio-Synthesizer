﻿#r @"C:\Users\itv-admin\.nuget\packages\xplot.plotly\4.0.3\lib\netstandard2.0\XPlot.Plotly.dll"

open XPlot.Plotly 

 let Bolivia = ["2004/05", 165.; "2005/06", 135.; "2006/07", 157.; "2007/08", 139.; "2008/09", 136.]
 let Ecuador = ["2004/05", 938.; "2005/06", 1120.; "2006/07", 1167.; "2007/08", 1110.; "2008/09", 691.]
 let Madagascar = ["2004/05", 522.; "2005/06", 599.; "2006/07", 587.; "2007/08", 615.; "2008/09", 629.]
 let Average = ["2004/05", 614.6; "2005/06", 682.; "2006/07", 623.; "2007/08", 609.4; "2008/09", 569.6]

 type HobbsPearsonTrial =
  { Radial : float list
    Angular : obj list
    Color : string }

 let HobbsPearson =
  [ { Radial = [6.804985785265978; 3.389596010612268; 5.3814721107464445; 8.059540219420184; 5.318229227868589; 2.9850999356273773; 1.9665870023752283; 6.769265408206589; 4.073401898721205; 6.50437182526841; 7.556369818996649; 4.047456094066775; 7.386662496070009; 5.413624736983931; 7.470716531163242; 7.982110216939738; 4.737814080093381; 4.206453042929911; 5.478604804594065; 4.824520280697772; 5.599600609899737; 6.8667952170824735; 3.0856713662561464; 7.771810943227382; 3.6877944350967193; 5.360356685192225; 5.140446739300986; 6.045445680928888; 6.833920940193708; 3.6207694625408364; 3.9894305834039687; 5.3118244995018; 4.608213480282062; 6.640584716151912; 3.055188854482986; 7.492564163752965; 5.4850781777896715; 3.8977949966209358; 5.976245114026165; 5.447061560910957; 5.37703411681004; 4.690805787731301; 4.711640491184845; 3.629919329394875; 5.957668076372498; 5.357121284391151; 3.849235282821748; 6.250507136319218; 7.122243357145468; 3.399404233835391; 3.5105566722713313; 4.100997603660974; 4.096382100199779; 6.233583074805102; 3.939488526772935; 3.9254450773976983; 6.118132501462698; 3.9404503462852323; 7.583015573261159; 3.513202145338516];
      Angular = [-30.352944361883697; -25.611459854524096; -12.425227452676078; 13.96138051872652; -4.9509328406707445; -25.692274190905437; 12.46876416157031; -4.913764107032951; -10.967380287631935; 30.814194054910676; 2.4749594311442737; 17.97554375239156; 0.7711305933623585; 6.137488485631386; -14.451963574013497; 28.184534112915948; 12.538680065954864; -8.983230337131154; 5.231285164762417; -64.48900253584051; 11.357486681772649; 3.4540747915125176; 13.924346613092862; -25.364002046782343; -16.81800638602268; -10.260051030559755; -13.212134125591882; 2.5793388653025744; 8.717574965852519; -10.675498719239487; -2.926366012522306; 25.195880754767717; 40.59032932155964; -9.121433630189772; -24.297362381339184; -3.1769445056889345; 10.85049841917252; -31.33205974736701; 4.849567462214266; 15.048276954124187; 3.2951046992599635; -6.197091873129837; -8.77857413578066; 29.549174119407287; -5.1374487928814645; 23.02686048794348; -6.634816578371129; 2.7550149918614695; 21.733250113653973; -24.816994960101756; -7.83054706253201; 28.325796210205855; 12.300977467795988; -21.563157240034112; -19.335516283813288; 26.146443170846787; -1.7060712026841085; 16.071723694996702; 2.053266302846965; -5.097911612332572];
      Color = "rgb(27,158,119)" }
    { Radial = [3.488043923008057; 2.9184785763552368; 4.201827359971069; 8.227324606851074; 4.776690427237194; 3.041912303114453; 4.789947719076336; 5.663880780360856; 3.858262393172743; 8.260212881141047; 6.868624486428106; 5.7401975996748895; 6.594979282458134; 5.692703778211614; 5.337916574462772; 9.283604185175781; 5.7645908931363365; 4.028864552051332; 5.662344748373121; 0.42283723110061455; 6.201266463929336; 6.43926538131984; 5.096758513060891; 4.632081908733815; 3.4218461363102217; 4.369404703352921; 4.02833441941273; 5.80576719754376; 6.848189921425055; 3.8092955127795802; 4.385268183833586; 6.98332684554596; 7.396273186029126; 5.215125003141; 3.0861487792429205; 6.335394491488218; 6.0904147140584834; 2.4480560069033306; 5.942784020305152; 6.373129885590045; 5.454205341176391; 4.393337616563476; 4.2059446799773; 6.155542287959513; 5.119087171162872; 6.869860830828341; 4.104599860575049; 5.954348125582761; 8.092332877153778; 2.9617697054526295; 3.974012187582175; 6.373384128907529; 5.415409143179902; 3.876890919980343; 3.261446947424557; 6.145808529699159; 5.502451987192818; 5.571553295311899; 6.853049261089887; 4.140355074942654];
      Angular = [14.80662578088746; 79.00634037258273; 49.02206554130045; 49.69908313603149; 54.137491082859476; 86.41932102054662; 96.95239193571373; 41.463488263612184; 67.13769169339066; 68.06103943971128; 42.68193032273406; 76.39865660811795; 42.19479347220856; 59.57788897461255; 27.510866799296068; 60.7534448322685; 68.37083279914752; 65.74802814945305; 58.53300837209963; -176.7441064584909; 61.17401857996598; 47.4515085890397; 84.4266531857914; 12.479346550525074; 72.48080276184626; 50.578831757750606; 51.560228240214684; 52.43785618126272; 51.586827992137934; 73.87294477733714; 70.21705692787259; 70.71429915430754; 82.23439442637098; 38.93539044700985; 84.7093666701594; 38.16582843645038; 61.70405365378903; 70.19695629244305; 54.454292590141606; 64.33489496861428; 58.273893146586325; 60.49982239038519; 59.155232538950266; 83.86561846759426; 47.87340989732011; 69.28260156593979; 71.1899104286971; 51.04839646304676; 59.427582415206295; 78.59873696166098; 75.75586451521559; 79.97048372322382; 73.89378024632016; 31.733411131690488; 68.08475117701943; 80.41107997857199; 48.92425070886502; 76.65025575535202; 42.18286436288056; 76.03333589453311];
      Color = "rgb(217,95,2)" }
    { Radial = [1.855870835032611; 5.2869620620428215; 3.886013391943573; 6.282863313001057; 4.4534148477405155; 5.688008050761193; 7.330864282608489; 3.825660594787748; 4.989604176963506; 7.8974314697670955; 4.6566931130229525; 6.667153696311044; 4.4310062871369515; 5.346113253377259; 2.4799456958789263; 8.113477348526397; 6.081311682312096; 4.968216896207305; 5.24445392063028; 5.422207884171506; 5.792774616023354; 4.787580592225452; 6.784318637182092; 1.10893690948093; 5.138911105244; 4.042929657287297; 4.022892029681135; 4.828428791305017; 5.417378374307972; 5.378635210668265; 5.421097175459842; 7.1205619788552434; 8.349308539903586; 3.4104855883231204; 5.6283784708757345; 3.9149369761396855; 5.7639402623551526; 4.764374106798512; 5.076236267895072; 6.165558183200791; 5.105576516279834; 4.761036376933375; 4.5962495409437905; 7.504188411346776; 4.107031417919988; 6.920422299379973; 5.349128949563397; 4.798065719385607; 7.023251532304466; 5.283680965457643; 5.569071152430292; 7.383794908447646; 6.269233210443127; 2.656529645009885; 4.843984338804117; 7.247992361555254; 4.372959394411489; 6.570981081360247; 4.602479243892371; 5.6700520508263965];
      Angular = [151.29425518111282; 147.1880250282001; 125.2821571123002; 87.0672979717481; 119.62789835678657; 147.74082414730458; 139.56459814488954; 101.39149710201973; 134.5601842795838; 104.02444470480259; 89.39314294483763; 123.19403140008181; 91.47434051519816; 113.33237361373538; 96.1499255673322; 93.28073452263222; 118.21556522583221; 132.32293737819631; 112.9411863909871; -179.74623313781393; 110.3035135586484; 97.7508361660772; 131.60808925703367; 115.49691923085226; 140.58118216037175; 123.39666211932598; 128.34200904453573; 107.6088103983526; 97.90468978746796; 137.12844797536522; 130.43124491245027; 112.2270844807976; 118.63020224581525; 106.05822558950737; 146.90810970600344; 90.27734955816595; 111.50528236323856; 151.0897425364386; 107.7213941567982; 111.30085499702685; 114.68027793629503; 126.56937949315531; 128.2189522328928; 125.35485719537647; 112.4180682532985; 111.79735567917727; 133.41805225814235; 105.18411684151033; 97.23103612064705; 146.66803680360073; 136.23931520140337; 121.79184419346764; 123.91132797113666; 129.86224497019268; 141.34395084996186; 123.27096774880371; 108.45882172345216; 124.41237705630053; 89.02711073868319; 134.8767011451154];
      Color = "rgb(117,112,179)" }
    { Radial = [5.3724709243191295; 7.096355572040467; 4.883823903200083; 2.9201354412366496; 4.723963045684014; 7.423693950928521; 8.090946075397593; 3.3068445913715996; 6.050828482522597; 5.530232074438094; 2.47230695264053; 6.275670536862141; 2.6158961737877817; 4.653539944582694; 3.3354400138758; 4.795883604868761; 5.472711346482787; 5.881930490947868; 4.5715870720453795; 9.039861169796675; 4.6429075998956915; 3.1727677357988284; 7.044248138818528; 4.466336514107385; 6.557330289803022; 4.8208494372533615; 5.131915515212963; 3.9700122370488873; 3.4063238128284303; 6.476722963998372; 6.019218509330762; 5.664501534954291; 7.1587585225456705; 3.6007126616736462; 7.324127168758531; 2.552946156245396; 4.727133860387479; 6.971755207182515; 4.076578361066991; 4.946223407006624; 4.642155449043171; 5.3605748644110855; 5.391719067363011; 7.072524305096543; 4.101111570277392; 5.485732621016895; 6.192535286114146; 3.7687113918423396; 4.290311389760529; 7.060195369692179; 6.539691844176445; 6.679744406490943; 6.060825358695814; 4.786574040927106; 6.416686529666599; 6.70328133338789; 3.8888478104797555; 6.308591081194454; 2.4370447709043273; 6.508186347897975];
      Angular = [-140.20332764140605; -168.084245433406; -166.2851413292181; 138.24886675310003; -174.4243864364084; -169.96048275947723; 176.9918226866201; -169.90141624864253; -172.64158159443713; 142.9516688139347; 172.4157463673128; 168.5193591959272; 177.82205369393654; 172.85519034865231; -146.01452170111628; 128.1772930242011; 169.16707278067625; -173.58857378893256; 173.72699270456877; -151.20610477226074; 166.26047716274937; 172.50756608236046; 173.9491839042747; -131.80684093766672; -170.63527383147678; -168.57708548315375; -166.76550342128579; 176.07048734819648; 162.29750149829133; -174.05574631254976; -178.06092985664986; 156.47126885027095; 155.23914214477145; -163.00052639405448; -170.116713265192; -170.63927248749107; 167.38314369359566; -163.0988170562564; 172.8807370063752; 163.38600768186703; 176.1825419773446; -174.579680173718; -172.33584488196067; 165.33802569398378; -172.52566426066147; 157.54287773943665; -175.88151109326037; 175.42764399370765; 142.06967472256432; -168.3407340189972; -175.8058311226083; 163.06374541935153; 171.72097499708474; -151.40390456860604; -168.2713690903466; 165.04532787828478; -177.3153366647533; 170.04241289697416; 173.59919660957283; -177.25065674571294];
      Color = "rgb(231,41,138)" }
    { Radial = [7.937557871379145; 7.302746491515634; 5.929302221442996; 2.407178713166243; 5.27092188705965; 7.400596127535688; 6.810820338360006; 4.967759034422344; 6.190229370454795; 2.158518657950606; 4.00412589386977; 4.776617321633007; 4.232250451808441; 4.30765487269422; 6.200275172864116; 0.7275138485344722; 4.378006803811767; 6.004964939443091; 4.341931702915758; 10.237982935327496; 3.8021588886978415; 3.96928117013756; 5.7589801424664335; 7.674179069144705; 6.699953533011802; 5.7343103881346; 6.044275915297742; 4.3129430660866035; 3.3775452824133043; 6.367666727269062; 5.737244181549697; 3.3963514719893415; 4.216467481387725; 5.464885016717265; 7.311135577533859; 4.745400769362272; 3.9164685318876504; 7.6029729903258385; 4.125204829441439; 3.6767949496501635; 4.551235788519779; 5.606960531523096; 5.794844257485189; 5.030528155694793; 5.109586240991219; 3.4054402079637396; 6.02630612538526; 4.2211092636354195; 1.9097829365788486; 7.254669393921678; 6.268875872033599; 4.56258056659493; 4.9180579654382806; 6.83656096252698; 6.78648654914422; 4.751014334485786; 4.719926347642004; 4.9278052151809675; 4.059190587394083; 6.128338984290388];
      Angular = [-101.83378577584543; -127.47839157875458; -112.2442849973417; -82.32591087119675; -114.6888556206928; -130.53786336160334; -145.0102649759552; -98.7488450072409; -124.44174882126121; -152.45411926998403; -89.29423655225057; -139.83245171792495; -91.54359518437012; -119.44216300369413; -92.45583852737828; -129.6599243163198; -131.0512350992248; -123.85291745359059; -118.08673900439605; -121.97921713765797; -121.91502996793754; -99.36184757774758; -141.46770199726927; -93.5662631891479; -126.33690140499776; -112.8349441777883; -114.38647992914663; -109.79607232724634; -102.74326471243563; -128.2467289067651; -127.79209264323043; -142.47362974536523; -161.58729418706835; -99.94061077957295; -130.16311732570668; -90.22881200957039; -122.65049121443685; -123.26775057177692; -111.99730880084306; -127.52831680551732; -117.93129533779559; -120.39163424547179; -119.38687147866949; -149.6746954924951; -107.85051750555007; -138.98993134073962; -127.5954702142739; -107.32083544041386; -117.5738074233824; -127.48166096847307; -129.91203316621693; -148.49521167061027; -135.33164137019145; -104.42165927641673; -123.87544021115426; -146.81682661802307; -107.0584854241401; -138.9025648732907; -88.89688251951031; -130.75446735589105];
      Color = "rgb(102,166,30)" }
    { Radial = [8.46918052789063; 5.821997567373959; 6.1409183282181425; 5.831724284786043; 5.546754471857236; 5.62748770920125; 3.94832897601986; 6.490184614609525; 5.320618245151644; 3.2435930414929843; 6.444085331576761; 3.363778100648707; 6.463116810505407; 4.730944925781221; 7.796578411114142; 4.570127829915901; 3.926206816002813; 5.254348139870139; 4.8384111066133375; 8.694523998982934; 4.39953181821818; 5.8564839051788535; 3.62157703921442; 8.894912373110186; 5.494542836078211; 5.968980890853802; 6.047899573604184; 5.384671396722035; 5.381220018196653; 5.11157462273727; 4.7705611050578; 3.0983308826347407; 1.665083171936659; 6.740258533332946; 5.5944949288820025; 6.879630825669177; 4.382792466280775; 6.410843616485085; 5.154204317772818; 4.01515851865648; 4.9391488682598155; 5.298297314485713; 5.490417176946796; 2.6237512593812404; 5.9535886616652665; 3.3014793719195046; 4.954889001100974; 5.500053669614178; 4.4505123495497285; 5.786624513349857; 4.906834424064605; 2.6299694734469274; 3.769703608047238; 7.396735715500286; 5.7644819019579545; 2.794585195883112; 5.782033269824353; 3.4853519176219963; 6.500653598620165; 4.748640710129176];
      Angular = [-66.53583632728323; -84.514422676922; -63.339741699567846; -24.146812744223833; -59.70124532256676; -88.06537267996578; -98.44420453532204; -49.15839681719936; -73.63622331202959; -17.923874678608904; -38.41239945460549; -66.34036237792131; -40.88883873919996; -52.46063321002169; -52.61046255912479; -7.039351050913894; -57.23545869215697; -71.64220350197985; -52.345396169095466; -92.78303867354904; -47.18716305503351; -41.969208462875166; -82.14422824993427; -59.43916560317718; -79.19482259319774; -62.29990853531319; -65.53790403937941; -48.9060554475786; -37.74831103800929; -78.05333345828834; -71.87311766307504; -41.891092825900685; -53.11545548549721; -52.997628097314845; -87.0843610179252; -43.61190483837573; -48.79799840560851; -82.56680315713163; -47.90996299570176; -46.57048558531105; -54.5004832176089; -65.90072712679752; -66.87331746360131; -75.48080725209734; -54.777693866880114; -42.5983345913628; -74.50816626907293; -47.11021844342552; -22.356873183328428; -84.19298674498425; -78.50528475620209; -65.0363717923471; -66.51373368133282; -63.52677656175937; -77.80907855131592; -68.51017974013602; -51.296869310885135; -68.33991302765452; -38.631733068443026; -77.85184858511114];
      Color = "rgb(230,171,2)" } ]

 let traces =
  [ for trial in HobbsPearson ->
    Scatter(r = trial.Radial, t = trial.Angular,
      mode = "markers", marker = Marker(color=trial.Color))]

 let layout =
  Layout(title = "Hobbs-Pearson Trials", showlegend = false,
    plot_bgcolor = "rgb(223,223,223)")

 let chart1 =
  traces
  |> Chart.Plot
  |> Chart.WithLayout layout