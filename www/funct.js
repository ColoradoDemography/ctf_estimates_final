//Website functions for State Demography Office webpage
//A. Bickford 5/2021

//list of lookup statements  https://github.com/ColoradoDemography/MS_Demog_Lookups/tree/master/doc
// profilesql syntax https://gis.dola.colorado.gov/lookups/profilesql?table=estimates.firm_count&year=2011&geo=1
//String FIPS codes need to be quoted e.g. '001'

//cat Utility Functions

function changeKeyObjects(arr, replaceKeys) {
//changeKeyObjects changes names in data files

  return arr.map(item => {
    const newItem = {};
    Object.keys(item).forEach(key => {
      newItem[replaceKeys[key]] = item[[key]];
    });
    return newItem;
  });
} 
// changeKeyObjects



function hideButtons() {
//hideButtons  selects and hides all "dropbtn" class buttons in DOM
	btn = document.getElementsByClassName("dropbtn"); 
	for(i = 0; i < btn.length; i++){
		btn[i].style.display = "none";
	};
}
// hideButtons

function showButtons() {
//showButtons  selects and shows all "dropbtn" class buttons in DOM
	btn = document.getElementsByClassName("dropbtn"); 
	for(i = 0; i < btn.length; i++){
		if(btn[i].style.display = "none") {
		btn[i].style.display = "block";
		}
	};
}
// showButtons

function accordionFun() {
//accordionFun  manages the accordion  panels in the DOM
var acc = document.getElementsByClassName("accordion");
var i;
for (i = 0; i < acc.length; i++) {
  acc[i].addEventListener("click", function() {
// Toggle between adding and removing the "active" class,
// to highlight the button that controls the panel 
    this.classList.toggle("active");

    // Toggle between hiding and showing the active panel 
    var panel = this.nextElementSibling;
    if (panel.style.display === "block") {
      panel.style.display = "none";
    } else {
      panel.style.display = "block";
    }
  });
}
} 
// accordionFun



function includeHTML() {
//includeHTML Manages inclusion of secondary pages. Taken from W3  https://www.w3schools.com/howto/howto_html_include.asp'
  var z, i, elmnt, file, xhttp;
  /* Loop through a collection of all HTML elements: */
  z = document.getElementsByTagName("*");
  
  for (i = 0; i < z.length; i++) {
    elmnt = z[i];
    /*search for elements with a certain atrribute:*/
    file = elmnt.getAttribute("w3-include-html");
    if (file) {
      /* Make an HTTP request using the attribute value as the file name: */
      xhttp = new XMLHttpRequest();
      xhttp.onreadystatechange = function() {
        if (this.readyState == 4) {
          if (this.status == 200) {elmnt.innerHTML = this.responseText;}
          if (this.status == 404) {elmnt.innerHTML = "Page not found.";}
          /* Remove the attribute, and call this function once more: */
          elmnt.removeAttribute("w3-include-html");
          includeHTML();
        }
      }
      xhttp.open("GET", file, true);
      xhttp.send();
      /* Exit the function: */
      return;
    }
  }
} 
// includeHTML


function transpose(data) {
//Tranposes data arrays
  let result = {};
  for (let row of data) {
    for (let [key, value] of Object.entries(row)) {
      result[key] = result[key] || [];
	result[key].push([{'name' : key, 'value' : value}]); 
    }
  }
  return result;
}; 
// transpose

function annot(annTxt,ypos) {
//annot  Chart annotation Places chart source citation on Plotly Charts

	const fmt_date = d3.timeFormat("%B %d, %Y");
		ypos = -0.23;
	
	var  outAnnot = {text :  annTxt +'  Print Date: ' +  fmt_date(new Date) , 
                font: {
    size : 9,
    color: 'black'
      },
      xref : 'paper', 
      x : 0, 
      yref : 'paper', 
      y : ypos, 
      align : 'left', 
      showarrow : false};
	  
return(outAnnot);
}
// annot

function muni_county(muni){
//muni_county provides county designation for municipalities (based on largest population for multi-county munis
var cty_n;
if(muni == '00760'){cty_n = '071'};
if(muni == '00925'){cty_n = '121'};
if(muni == '01090'){cty_n = '003'};
if(muni == '01530'){cty_n = '093'};
if(muni == '02355'){cty_n = '021'};
if(muni == '03235'){cty_n = '073'};
if(muni == '03455'){cty_n = '059'};
if(muni == '03620'){cty_n = '097'};
if(muni == '03950'){cty_n = '123'};
if(muni == '04000'){cty_n = '005'};
if(muni == '04110'){cty_n = '037'};
if(muni == '04935'){cty_n = '037'};
if(muni == '05265'){cty_n = '067'};
if(muni == '06090'){cty_n = '001'};
if(muni == '06255'){cty_n = '069'};
if(muni == '06530'){cty_n = '063'};
if(muni == '07025'){cty_n = '047'};
if(muni == '07190'){cty_n = '023'};
if(muni == '07410'){cty_n = '117'};
if(muni == '07571'){cty_n = '109'};
if(muni == '07795'){cty_n = '101'};
if(muni == '07850'){cty_n = '013'};
if(muni == '08070'){cty_n = '005'};
if(muni == '08345'){cty_n = '071'};
if(muni == '08400'){cty_n = '117'};
if(muni == '08675'){cty_n = '001'};
if(muni == '09115'){cty_n = '043'};
if(muni == '09280'){cty_n = '014'};
if(muni == '09555'){cty_n = '087'};
if(muni == '10105'){cty_n = '015'};
if(muni == '10600'){cty_n = '063'};
if(muni == '11260'){cty_n = '041'};
if(muni == '11645'){cty_n = '009'};
if(muni == '11810'){cty_n = '043'};
if(muni == '12045'){cty_n = '045'};
if(muni == '12080'){cty_n = '045'};
if(muni == '12387'){cty_n = '035'};
if(muni == '12415'){cty_n = '035'};
if(muni == '12635'){cty_n = '029'};
if(muni == '12815'){cty_n = '005'};
if(muni == '12855'){cty_n = '109'};
if(muni == '12910'){cty_n = '047'};
if(muni == '13460'){cty_n = '089'};
if(muni == '13845'){cty_n = '005'};
if(muni == '14175'){cty_n = '017'};
if(muni == '14765'){cty_n = '079'};
if(muni == '15330'){cty_n = '043'};
if(muni == '15550'){cty_n = '071'};
if(muni == '15605'){cty_n = '077'};
if(muni == '16000'){cty_n = '041'};
if(muni == '16385'){cty_n = '005'};
if(muni == '16495'){cty_n = '001'};
if(muni == '17375'){cty_n = '083'};
if(muni == '17760'){cty_n = '081'};
if(muni == '17925'){cty_n = '029'};
if(muni == '18310'){cty_n = '051'};
if(muni == '18420'){cty_n = '109'};
if(muni == '18530'){cty_n = '119'};
if(muni == '18640'){cty_n = '075'};
if(muni == '18750'){cty_n = '025'};
if(muni == '19080'){cty_n = '123'};
if(muni == '19355'){cty_n = '077'};
if(muni == '19630'){cty_n = '005'};
if(muni == '19795'){cty_n = '105'};
if(muni == '19850'){cty_n = '029'};
if(muni == '20000'){cty_n = '031'};
if(muni == '20440'){cty_n = '117'};
if(muni == '20495'){cty_n = '081'};
if(muni == '20770'){cty_n = '083'};
if(muni == '21265'){cty_n = '033'};
if(muni == '22035'){cty_n = '067'};
if(muni == '22145'){cty_n = '061'};
if(muni == '22200'){cty_n = '037'};
if(muni == '22860'){cty_n = '123'};
if(muni == '23025'){cty_n = '125'};
if(muni == '23135'){cty_n = '059'};
if(muni == '23740'){cty_n = '039'};
if(muni == '24620'){cty_n = '019'};
if(muni == '24785'){cty_n = '005'};
if(muni == '24950'){cty_n = '123'};
if(muni == '25115'){cty_n = '069'};
if(muni == '25280'){cty_n = '123'};
if(muni == '25610'){cty_n = '093'};
if(muni == '26270'){cty_n = '001'};
if(muni == '26600'){cty_n = '123'};
if(muni == '26765'){cty_n = '063'};
if(muni == '26875'){cty_n = '075'};
if(muni == '27040'){cty_n = '043'};
if(muni == '27425'){cty_n = '069'};
if(muni == '27700'){cty_n = '123'};
if(muni == '27810'){cty_n = '087'};
if(muni == '27865'){cty_n = '041'};
if(muni == '27975'){cty_n = '089'};
if(muni == '28105'){cty_n = '005'};
if(muni == '28305'){cty_n = '049'};
if(muni == '28360'){cty_n = '123'};
if(muni == '28690'){cty_n = '117'};
if(muni == '28745'){cty_n = '077'};
if(muni == '29185'){cty_n = '123'};
if(muni == '29680'){cty_n = '073'};
if(muni == '29735'){cty_n = '019'};
if(muni == '29955'){cty_n = '123'};
if(muni == '30340'){cty_n = '005'};
if(muni == '30780'){cty_n = '045'};
if(muni == '30835'){cty_n = '059'};
if(muni == '31550'){cty_n = '099'};
if(muni == '31605'){cty_n = '049'};
if(muni == '31660'){cty_n = '077'};
if(muni == '31715'){cty_n = '049'};
if(muni == '32155'){cty_n = '123'};
if(muni == '32650'){cty_n = '041'};
if(muni == '33035'){cty_n = '005'};
if(muni == '33310'){cty_n = '123'};
if(muni == '33640'){cty_n = '051'};
if(muni == '33695'){cty_n = '037'};
if(muni == '34520'){cty_n = '099'};
if(muni == '34740'){cty_n = '061'};
if(muni == '34960'){cty_n = '095'};
if(muni == '35070'){cty_n = '107'};
if(muni == '36610'){cty_n = '087'};
if(muni == '37215'){cty_n = '099'};
if(muni == '37270'){cty_n = '095'};
if(muni == '37380'){cty_n = '003'};
if(muni == '37545'){cty_n = '029'};
if(muni == '37600'){cty_n = '049'};
if(muni == '37820'){cty_n = '123'};
if(muni == '37875'){cty_n = '073'};
if(muni == '38370'){cty_n = '019'};
if(muni == '38535'){cty_n = '067'};
if(muni == '38590'){cty_n = '075'};
if(muni == '39195'){cty_n = '013'};
if(muni == '39855'){cty_n = '123'};
if(muni == '39965'){cty_n = '115'};
if(muni == '40185'){cty_n = '123'};
if(muni == '40515'){cty_n = '123'};
if(muni == '40570'){cty_n = '071'};
if(muni == '40790'){cty_n = '039'};
if(muni == '41010'){cty_n = '017'};
if(muni == '41560'){cty_n = '049'};
if(muni == '41835'){cty_n = '013'};
if(muni == '42055'){cty_n = '021'};
if(muni == '42110'){cty_n = '089'};
if(muni == '42330'){cty_n = '053'};
if(muni == '42495'){cty_n = '059'};
if(muni == '43000'){cty_n = '059'};
if(muni == '43110'){cty_n = '099'};
if(muni == '43550'){cty_n = '035'};
if(muni == '43605'){cty_n = '123'};
if(muni == '43660'){cty_n = '011'};
if(muni == '44100'){cty_n = '055'};
if(muni == '44320'){cty_n = '065'};
if(muni == '44980'){cty_n = '073'};
if(muni == '45255'){cty_n = '005'};
if(muni == '45530'){cty_n = '123'};
if(muni == '45695'){cty_n = '087'};
if(muni == '45955'){cty_n = '035'};
if(muni == '45970'){cty_n = '013'};
if(muni == '46355'){cty_n = '013'};
if(muni == '46465'){cty_n = '069'};
if(muni == '47070'){cty_n = '013'};
if(muni == '48060'){cty_n = '021'};
if(muni == '48115'){cty_n = '083'};
if(muni == '48445'){cty_n = '041'};
if(muni == '48500'){cty_n = '089'};
if(muni == '48555'){cty_n = '051'};
if(muni == '49600'){cty_n = '123'};
if(muni == '49875'){cty_n = '103'};
if(muni == '50040'){cty_n = '075'};
if(muni == '50480'){cty_n = '123'};
if(muni == '50920'){cty_n = '037'};
if(muni == '51250'){cty_n = '109'};
if(muni == '51635'){cty_n = '105'};
if(muni == '51690'){cty_n = '117'};
if(muni == '51745'){cty_n = '085'};
if(muni == '51800'){cty_n = '041'};
if(muni == '52075'){cty_n = '059'};
if(muni == '52350'){cty_n = '059'};
if(muni == '52550'){cty_n = '113'};
if(muni == '52570'){cty_n = '051'};
if(muni == '53120'){cty_n = '085'};
if(muni == '53175'){cty_n = '013'};
if(muni == '53395'){cty_n = '045'};
if(muni == '54330'){cty_n = '001'};
if(muni == '54880'){cty_n = '113'};
if(muni == '54935'){cty_n = '085'};
if(muni == '55045'){cty_n = '123'};
if(muni == '55155'){cty_n = '107'};
if(muni == '55540'){cty_n = '085'};
if(muni == '55705'){cty_n = '025'};
if(muni == '55870'){cty_n = '113'};
if(muni == '55980'){cty_n = '029'};
if(muni == '56145'){cty_n = '025'};
if(muni == '56365'){cty_n = '121'};
if(muni == '56420'){cty_n = '091'};
if(muni == '56475'){cty_n = '115'};
if(muni == '56860'){cty_n = '007'};
if(muni == '56970'){cty_n = '077'};
if(muni == '57025'){cty_n = '041'};
if(muni == '57245'){cty_n = '095'};
if(muni == '57300'){cty_n = '029'};
if(muni == '57400'){cty_n = '045'};
if(muni == '57630'){cty_n = '035'};
if(muni == '58235'){cty_n = '075'};
if(muni == '59005'){cty_n = '123'};
if(muni == '59830'){cty_n = '051'};
if(muni == '60160'){cty_n = '123'};
if(muni == '60600'){cty_n = '015'};
if(muni == '61315'){cty_n = '009'};
if(muni == '62000'){cty_n = '101'};
if(muni == '62660'){cty_n = '041'};
if(muni == '62880'){cty_n = '103'};
if(muni == '63045'){cty_n = '123'};
if(muni == '63265'){cty_n = '037'};
if(muni == '64090'){cty_n = '033'};
if(muni == '64200'){cty_n = '091'};
if(muni == '64255'){cty_n = '045'};
if(muni == '64970'){cty_n = '043'};
if(muni == '65190'){cty_n = '089'};
if(muni == '65740'){cty_n = '021'};
if(muni == '66895'){cty_n = '101'};
if(muni == '67005'){cty_n = '109'};
if(muni == '67280'){cty_n = '015'};
if(muni == '67830'){cty_n = '021'};
if(muni == '68105'){cty_n = '023'};
if(muni == '68655'){cty_n = '113'};
if(muni == '68930'){cty_n = '115'};
if(muni == '69040'){cty_n = '063'};
if(muni == '69150'){cty_n = '123'};
if(muni == '69645'){cty_n = '005'};
if(muni == '69700'){cty_n = '061'};
if(muni == '70195'){cty_n = '045'};
if(muni == '70250'){cty_n = '027'};
if(muni == '70360'){cty_n = '019'};
if(muni == '70525'){cty_n = '117'};
if(muni == '70580'){cty_n = '111'};
if(muni == '70635'){cty_n = '039'};
if(muni == '71755'){cty_n = '097'};
if(muni == '72395'){cty_n = '105'};
if(muni == '73330'){cty_n = '009'};
if(muni == '73715'){cty_n = '071'};
if(muni == '73825'){cty_n = '107'};
if(muni == '73935'){cty_n = '075'};
if(muni == '74485'){cty_n = '063'};
if(muni == '74815'){cty_n = '025'};
if(muni == '75640'){cty_n = '013'};
if(muni == '75970'){cty_n = '089'};
if(muni == '76795'){cty_n = '113'};
if(muni == '77290'){cty_n = '001'};
if(muni == '77510'){cty_n = '069'};
if(muni == '78610'){cty_n = '071'};
if(muni == '79270'){cty_n = '009'};
if(muni == '80040'){cty_n = '037'};
if(muni == '80865'){cty_n = '119'};
if(muni == '81030'){cty_n = '009'};
if(muni == '81690'){cty_n = '063'};
if(muni == '82130'){cty_n = '057'};
if(muni == '82350'){cty_n = '055'};
if(muni == '82460'){cty_n = '009'};
if(muni == '82735'){cty_n = '013'};
if(muni == '83230'){cty_n = '069'};
if(muni == '83450'){cty_n = '027'};
if(muni == '83835'){cty_n = '001'};
if(muni == '84440'){cty_n = '059'};
if(muni == '84770'){cty_n = '087'};
if(muni == '85045'){cty_n = '099'};
if(muni == '85155'){cty_n = '043'};
if(muni == '85485'){cty_n = '123'};
if(muni == '85705'){cty_n = '049'};
if(muni == '86090'){cty_n = '119'};
if(muni == '86310'){cty_n = '125'};
if(muni == '86475'){cty_n = '107'};
if(muni == '86750'){cty_n = '125'};
return cty_n;
} 
// muni_county
   

function cdp_county(cdp) {
//cdp_county returns county code for cdp
if(cdp == '06172'){cty = '001'};
if(cdp == '20275'){cty = '001'};
if(cdp == '54750'){cty = '001'};
if(cdp == '69480'){cty = '001'};
if(cdp == '69810'){cty = '001'};
if(cdp == '77757'){cty = '001'};
if(cdp == '79100'){cty = '001'};
if(cdp == '83120'){cty = '001'};
if(cdp == '74375'){cty = '001'};
if(cdp == '82905'){cty = '001'};
if(cdp == '01145'){cty = '003'};
if(cdp == '00620'){cty = '005'};
if(cdp == '08530'){cty = '005'};
if(cdp == '10985'){cty = '005'};
if(cdp == '13590'){cty = '005'};
if(cdp == '16465'){cty = '005'};
if(cdp == '21330'){cty = '005'};
if(cdp == '37220'){cty = '005'};
if(cdp == '38910'){cty = '005'};
if(cdp == '58510'){cty = '005'};
if(cdp == '16110'){cty = '005'};
if(cdp == '02905'){cty = '007'};
if(cdp == '34685'){cty = '011'};
if(cdp == '01420'){cty = '013'};
if(cdp == '01740'){cty = '013'};
if(cdp == '04620'){cty = '013'};
if(cdp == '07580'){cty = '013'};
if(cdp == '18585'){cty = '013'};
if(cdp == '23575'){cty = '013'};
if(cdp == '23630'){cty = '013'};
if(cdp == '30350'){cty = '013'};
if(cdp == '30945'){cty = '013'};
if(cdp == '33502'){cty = '013'};
if(cdp == '35860'){cty = '013'};
if(cdp == '44270'){cty = '013'};
if(cdp == '44695'){cty = '013'};
if(cdp == '52210'){cty = '013'};
if(cdp == '53780'){cty = '013'};
if(cdp == '57445'){cty = '013'};
if(cdp == '59240'){cty = '013'};
if(cdp == '67040'){cty = '013'};
if(cdp == '69110'){cty = '013'};
if(cdp == '74980'){cty = '013'};
if(cdp == '75585'){cty = '013'};
if(cdp == '76325'){cty = '013'};
if(cdp == '80370'){cty = '013'};
if(cdp == '15302'){cty = '013'};
if(cdp == '29295'){cty = '015'};
if(cdp == '39800'){cty = '015'};
if(cdp == '49490'){cty = '015'};
if(cdp == '71625'){cty = '015'};
if(cdp == '21390'){cty = '019'};
if(cdp == '27175'){cty = '019'};
if(cdp == '67142'){cty = '019'};
if(cdp == '79785'){cty = '019'};
if(cdp == '11975'){cty = '021'};
if(cdp == '16715'){cty = '021'};
if(cdp == '27535'){cty = '023'};
if(cdp == '67500'){cty = '023'};
if(cdp == '00320'){cty = '035'};
if(cdp == '12387'){cty = '035'};
if(cdp == '28250'){cty = '035'};
if(cdp == '31935'){cty = '035'};
if(cdp == '36410'){cty = '035'};
if(cdp == '46410'){cty = '035'};
if(cdp == '50012'){cty = '035'};
if(cdp == '58592'){cty = '035'};
if(cdp == '66197'){cty = '035'};
if(cdp == '68875'){cty = '035'};
if(cdp == '74080'){cty = '035'};
if(cdp == '77235'){cty = '035'};
if(cdp == '83500'){cty = '035'};
if(cdp == '21155'){cty = '037'};
if(cdp == '23300'){cty = '037'};
if(cdp == '23795'){cty = '037'};
if(cdp == '28830'){cty = '037'};
if(cdp == '47345'){cty = '037'};
if(cdp == '85760'){cty = '037'};
if(cdp == '00870'){cty = '041'};
if(cdp == '06970'){cty = '041'};
if(cdp == '12325'){cty = '041'};
if(cdp == '14587'){cty = '041'};
if(cdp == '24235'){cty = '041'};
if(cdp == '27370'){cty = '041'};
if(cdp == '30420'){cty = '041'};
if(cdp == '58675'){cty = '041'};
if(cdp == '64870'){cty = '041'};
if(cdp == '68847'){cty = '041'};
if(cdp == '74430'){cty = '041'};
if(cdp == '86117'){cty = '041'};
if(cdp == '23520'){cty = '039'};
if(cdp == '60655'){cty = '039'};
if(cdp == '15440'){cty = '043'};
if(cdp == '17485'){cty = '043'};
if(cdp == '37655'){cty = '043'};
if(cdp == '45145'){cty = '043'};
if(cdp == '58400'){cty = '043'};
if(cdp == '05120'){cty = '045'};
if(cdp == '12460'){cty = '045'};
if(cdp == '12470'){cty = '045'};
if(cdp == '12945'){cty = '045'};
if(cdp == '52820'){cty = '045'};
if(cdp == '53875'){cty = '045'};
if(cdp == '65685'){cty = '047'};
if(cdp == '57850'){cty = '049'};
if(cdp == '76190'){cty = '049'};
if(cdp == '12450'){cty = '053'};
if(cdp == '58960'){cty = '053'};
if(cdp == '02575'){cty = '059'};
if(cdp == '03730'){cty = '059'};
if(cdp == '19150'){cty = '059'};
if(cdp == '22575'){cty = '059'};
if(cdp == '25390'){cty = '059'};
if(cdp == '25550'){cty = '059'};
if(cdp == '29625'){cty = '059'};
if(cdp == '38480'){cty = '059'};
if(cdp == '38810'){cty = '059'};
if(cdp == '40377'){cty = '059'};
if(cdp == '41065'){cty = '059'};
if(cdp == '84042'){cty = '059'};
if(cdp == '08290'){cty = '061'};
if(cdp == '78335'){cty = '061'};
if(cdp == '72320'){cty = '067'};
if(cdp == '44375'){cty = '065'};
if(cdp == '79105'){cty = '065'};
if(cdp == '43220'){cty = '069'};
if(cdp == '63320'){cty = '069'};
if(cdp == '24290'){cty = '071'};
if(cdp == '36940'){cty = '071'};
if(cdp == '39250'){cty = '071'};
if(cdp == '47015'){cty = '071'};
if(cdp == '68985'){cty = '071'};
if(cdp == '74275'){cty = '071'};
if(cdp == '80095'){cty = '071'};
if(cdp == '84000'){cty = '071'};
if(cdp == '03840'){cty = '075'};
if(cdp == '56695'){cty = '075'};
if(cdp == '15165'){cty = '077'};
if(cdp == '28800'){cty = '077'};
if(cdp == '45750'){cty = '077'};
if(cdp == '56035'){cty = '077'};
if(cdp == '63375'){cty = '077'};
if(cdp == '49325'){cty = '081'};
if(cdp == '44595'){cty = '083'};
if(cdp == '78280'){cty = '083'};
if(cdp == '63705'){cty = '085'};
if(cdp == '07420'){cty = '087'};
if(cdp == '39160'){cty = '087'};
if(cdp == '51975'){cty = '087'};
if(cdp == '55925'){cty = '087'};
if(cdp == '66995'){cty = '087'};
if(cdp == '71790'){cty = '087'};
if(cdp == '78345'){cty = '087'};
if(cdp == '83175'){cty = '087'};
if(cdp == '42165'){cty = '089'};
if(cdp == '54495'){cty = '089'};
if(cdp == '15825'){cty = '091'};
if(cdp == '45680'){cty = '091'};
if(cdp == '60765'){cty = '091'};
if(cdp == '33420'){cty = '093'};
if(cdp == '01915'){cty = '095'};
if(cdp == '53945'){cty = '097'};
if(cdp == '63650'){cty = '097'};
if(cdp == '86200'){cty = '097'};
if(cdp == '04165'){cty = '101'};
if(cdp == '06602'){cty = '101'};
if(cdp == '07245'){cty = '101'};
if(cdp == '15935'){cty = '101'};
if(cdp == '62220'){cty = '101'};
if(cdp == '67445'){cty = '101'};
if(cdp == '81305'){cty = '101'};
if(cdp == '01640'){cty = '105'};
if(cdp == '29845'){cty = '105'};
if(cdp == '58758'){cty = '107'};
if(cdp == '17150'){cty = '117'};
if(cdp == '35400'){cty = '117'};
if(cdp == '40550'){cty = '117'};
if(cdp == '20605'){cty = '119'};
if(cdp == '27095'){cty = '119'};
if(cdp == '30890'){cty = '119'};
if(cdp == '50380'){cty = '119'};
if(cdp == '03015'){cty = '123'};
if(cdp == '38425'){cty = '125'};
if(cdp == '39745'){cty = '125'};
if(cdp == '40900'){cty = '125'};
if(cdp == '42000'){cty = '125'};
if(cdp == '80755'){cty = '125'};
return cty;
} 
// cdp_county

function RegionNum(nam) {
//RegionNum takes the names of selected regions and returns a list of region numbers
var regionNum = 0;
if(nam == 'Colorado') {regionNum = 0};
if(nam == 'Central Mountains') {regionNum = 1};
if(nam == 'Eastern Plains') {regionNum = 2};
if(nam == 'Front Range') {regionNum = 3};
if(nam == 'San Luis Valley') {regionNum = 4};
if(nam == 'Western Slope') {regionNum = 5};
if(nam == 'Region 1: Northern Eastern Plains') {regionNum = 6};
if(nam == 'Region 2: Northern Front Range') {regionNum = 7};
if(nam == 'Region 3: Denver Metropolitan Area') {regionNum = 8};
if(nam == 'Region 4: Southern Front Range') {regionNum = 9};
if(nam == 'Region 5: Central Eastern Plains') {regionNum = 10};
if(nam == 'Region 6: Southern Eastern Plains') {regionNum = 11};
if(nam == 'Region 7: Pueblo County') {regionNum = 12};
if(nam == 'Region 8: San Luis Valley') {regionNum = 13};
if(nam == 'Region 9: Southern Western Slope') {regionNum = 14};
if(nam == 'Region 10: Central Western Slope') {regionNum = 15};
if(nam == 'Region 11: Northern Western Slope') {regionNum = 16};
if(nam == 'Region 12: Northern Mountains') {regionNum = 17};
if(nam == 'Region 13: Central Mountains') {regionNum = 18};
if(nam == 'Region 14: Southern Mountains') {regionNum = 19};
if(nam == 'Boulder') {regionNum = 20};
if(nam == 'Colorado Springs') {regionNum = 21};
if(nam == 'Denver-Aurora-Lakewood') {regionNum = 22};
if(nam == 'Fort Collins') {regionNum = 23};
if(nam == 'Grand Junction') {regionNum = 24};
if(nam == 'Greeley') {regionNum = 25};
if(nam == 'Pueblo') {regionNum = 26};
if(nam == 'Breckenridge') {regionNum = 27};
if(nam == 'Ca\u00f1on City') {regionNum = 28};
if(nam == 'Craig') {regionNum = 29};
if(nam == 'Durango') {regionNum = 30};
if(nam == 'Edwards') {regionNum = 31};
if(nam == 'Fort Morgan') {regionNum = 32};
if(nam == 'Glenwood Springs') {regionNum = 33};
if(nam == 'Montrose') {regionNum = 34};
if(nam == 'Steamboat Springs') {regionNum = 35};
if(nam == 'Sterling') {regionNum = 36};
if(nam == 'Denver PMSA') {regionNum = 37};
if(nam == 'Denver-Boulder Metro Area') {regionNum = 38};
if(nam == 'Denver-Boulder-Greely CMSA') {regionNum = 39};

return(regionNum);
}; 
// RegionNum


function regionName(reg) {
//RegionName takes the region number and returns the name
if(reg == 0) {name =  'Colorado'};
if(reg == 1) {name =  'Central Mountains'};
if(reg == 2) {name =  'Eastern Plains'};
if(reg == 3) {name =  'Front Range'};
if(reg == 4) {name =  'San Luis Valley'};
if(reg == 5) {name =  'Western Slope'};
if(reg == 6) {name = 'Region 1: Northern Eastern Plains'};
if(reg == 7) {name = 'Region 2: Northern Front Range'};
if(reg == 8) {name = 'Region 3: Denver Metropolitan Area'};
if(reg == 9) {name = 'Region 4: Southern Front Range'};
if(reg == 10) {name = 'Region 5: Central Eastern Plains'};
if(reg == 11) {name = 'Region 6: Southern Eastern Plains'};
if(reg == 12) {name = 'Region 7: Pueblo County'};
if(reg == 13) {name = 'Region 8: San Luis Valley'};
if(reg == 14) {name = 'Region 9: Southern Western Slope'};
if(reg == 15) {name = 'Region 10: Central Western Slope'};
if(reg == 16) {name = 'Region 11: Northern Western Slope'};
if(reg == 17) {name = 'Region 12: Northern Mountains'};
if(reg == 18) {name = 'Region 13: Central Mountains'};
if(reg == 19) {name = 'Region 14: Southern Mountains'};
if(reg == 20) {name =  'Boulder'};
if(reg == 21) {name =  'Colorado Springs'};
if(reg == 22) {name =  'Denver-Aurora-Lakewood'};
if(reg == 23) {name =  'Fort Collins'};
if(reg == 24) {name =  'Grand Junction'};
if(reg == 25) {name =  'Greeley'};
if(reg == 26) {name =  'Pueblo'};
if(reg == 27) {name =  'Breckenridge'};
if(reg == 28) {name =  'Ca\u00f1on City'};
if(reg == 29) {name =  'Craig'};
if(reg == 30) {name =  'Durango'};
if(reg == 31) {name =  'Edwards'};
if(reg == 32) {name =  'Fort Morgan'};
if(reg == 33) {name =  'Glenwood Springs'};
if(reg == 34) {name = 'Montrose'};
if(reg == 35) {name =  'Steamboat Springs'};
if(reg == 36) {name =  'Sterling'};
if(reg == 37) {name =  'Denver PMSA'};
if(reg == 38) {name =  'Denver-Boulder Metro Area'};
if(reg == 39) {name =  'Denver-Boulder-Greely CMSA'};
return name;
}; 
// Regionname

function countyName(cty){
//countyName  Returns a county name from the numeric fips code
	if(cty == 0) {name = "Colorado"};
	if(cty == 1){name = 'Adams County'};
	if(cty == 3){name = 'Alamosa County'};
	if(cty == 5){name = 'Arapahoe County'};
	if(cty == 7){name = 'Archuleta County'};
	if(cty == 9){name = 'Baca County'};
	if(cty == 11){name = 'Bent County'};
	if(cty == 13){name = 'Boulder County'};
	if(cty == 14){name = 'Broomfield County'};
	if(cty == 15){name = 'Chaffee County'};
	if(cty == 17){name = 'Cheyenne County'};
	if(cty == 19){name = 'Clear Creek County'};
	if(cty == 21){name = 'Conejos County'};
	if(cty == 23){name = 'Costilla County'};
	if(cty == 25){name = 'Crowley County'};
	if(cty == 27){name = 'Custer County'};
	if(cty == 29){name = 'Delta County'};
	if(cty == 31){name = 'Denver County'};
	if(cty == 33){name = 'Dolores County'};
	if(cty == 35){name = 'Douglas County'};
	if(cty == 37){name = 'Eagle County'};
	if(cty == 39){name = 'Elbert County'};
	if(cty == 41){name = 'El Paso County'};
	if(cty == 43){name = 'Fremont County'};
	if(cty == 45){name = 'Garfield County'};
	if(cty == 47){name = 'Gilpin County'};
	if(cty == 49){name = 'Grand County'};
	if(cty == 51){name = 'Gunnison County'};
	if(cty == 53){name = 'Hinsdale County'};
	if(cty == 55){name = 'Huerfano County'};
	if(cty == 57){name = 'Jackson County'};
	if(cty == 59){name = 'Jefferson County'};
	if(cty == 61){name = 'Kiowa County'};
	if(cty == 63){name = 'Kit Carson County'};
	if(cty == 65){name = 'Lake County'};
	if(cty == 67){name = 'La Plata County'};
	if(cty == 69){name = 'Larimer County'};
	if(cty == 71){name = 'Las Animas County'};
	if(cty == 73){name = 'Lincoln County'};
	if(cty == 75){name = 'Logan County'};
	if(cty == 77){name = 'Mesa County'};
	if(cty == 79){name = 'Mineral County'};
	if(cty == 81){name = 'Moffat County'};
	if(cty == 83){name = 'Montezuma County'};
	if(cty == 85){name = 'Montrose County'};
	if(cty == 87){name = 'Morgan County'};
	if(cty == 89){name = 'Otero County'};
	if(cty == 91){name = 'Ouray County'};
	if(cty == 93){name = 'Park County'};
	if(cty == 95){name = 'Phillips County'};
	if(cty == 97){name = 'Pitkin County'};
	if(cty == 99){name = 'Prowers County'};
	if(cty == 101){name = 'Pueblo County'};
	if(cty == 103){name = 'Rio Blanco County'};
	if(cty == 105){name = 'Rio Grande County'};
	if(cty == 107){name = 'Routt County'};
	if(cty == 109){name = 'Saguache County'};
	if(cty == 111){name = 'San Luis County'};
	if(cty == 113){name = 'San Miguel County'};
	if(cty == 115){name = 'Sedgwick County'};
	if(cty == 117){name = 'Summit County'};
	if(cty == 119){name = 'Teller County'};
	if(cty == 121){name = 'Washington County'};
	if(cty == 123){name = 'Weld County'};
	if(cty == 125){name = 'Yuma County'};
	if(cty == 500){name = 'Denver-Boulder Metro Area'}
return name;
}; 
// countyName


function muniName(muni){
//muniName Returns Municipality name from Numeric FIPS code
	var name;
	if(muni == 0) {name = 'Colorado'};
	if(muni == 760){name = 'Aguilar'};
	if(muni == 925){name = 'Akron'};
	if(muni == 1090){name = 'Alamosa'};
	if(muni == 1530){name = 'Alma'};
	if(muni == 2355){name = 'Antonito'};
	if(muni == 3235){name = 'Arriba'};
	if(muni == 3455){name = 'Arvada'};
	if(muni == 3620){name = 'Aspen'};
	if(muni == 3950){name = 'Ault'};
	if(muni == 4000){name = 'Aurora'};
	if(muni == 4110){name = 'Avon'};
	if(muni == 4935){name = 'Basalt'};
	if(muni == 5265){name = 'Bayfield'};
	if(muni == 6090){name = 'Bennett'};
	if(muni == 6255){name = 'Berthoud'};
	if(muni == 6530){name = 'Bethune'};
	if(muni == 7025){name = 'Black Hawk'};
	if(muni == 7190){name = 'Blanca'};
	if(muni == 7410){name = 'Blue River'};
	if(muni == 7571){name = 'Bonanza'};
	if(muni == 7795){name = 'Boone'};
	if(muni == 7850){name = 'Boulder'};
	if(muni == 8070){name = 'Bow Mar'};
	if(muni == 8345){name = 'Branson'};
	if(muni == 8400){name = 'Breckenridge'};
	if(muni == 8675){name = 'Brighton'};
	if(muni == 9115){name = 'Brookside'};
	if(muni == 9280){name = 'Broomfield'};
	if(muni == 9555){name = 'Brush'};
	if(muni == 10105){name = 'Buena Vista'};
	if(muni == 10600){name = 'Burlington'};
	if(muni == 11260){name = 'Calhan'};
	if(muni == 11645){name = 'Campo'};
	if(muni == 11810){name = 'Cañon City'};
	if(muni == 12030){name = 'Carbonate'};
	if(muni == 12045){name = 'Carbondale'};
	if(muni == 12387){name = 'Castle Pines'};
	if(muni == 12415){name = 'Castle Rock'};
	if(muni == 12635){name = 'Cedaredge'};
	if(muni == 12815){name = 'Centennial'};
	if(muni == 12855){name = 'Center'};
	if(muni == 12910){name = 'Central City'};
	if(muni == 13460){name = 'Cheraw'};
	if(muni == 13845){name = 'Cherry Hills Village'};
	if(muni == 14175){name = 'Cheyenne Wells'};
	if(muni == 14765){name = 'City of Creede'};
	if(muni == 15330){name = 'Coal Creek'};
	if(muni == 15550){name = 'Cokedale'};
	if(muni == 15605){name = 'Collbran'};
	if(muni == 16000){name = 'Colorado Springs'};
	if(muni == 16385){name = 'Columbine Valley'};
	if(muni == 16495){name = 'Commerce City'};
	if(muni == 17375){name = 'Cortez'};
	if(muni == 17760){name = 'Craig'};
	if(muni == 17925){name = 'Crawford'};
	if(muni == 18310){name = 'Crested Butte'};
	if(muni == 18420){name = 'Crestone'};
	if(muni == 18530){name = 'Cripple Creek'};
	if(muni == 18640){name = 'Crook'};
	if(muni == 18750){name = 'Crowley'};
	if(muni == 19080){name = 'Dacono'};
	if(muni == 19355){name = 'De Beque'};
	if(muni == 19630){name = 'Deer Trail'};
	if(muni == 19795){name = 'Del Norte'};
	if(muni == 19850){name = 'Delta'};
	if(muni == 20000){name = 'Denver'};
	if(muni == 20440){name = 'Dillon'};
	if(muni == 20495){name = 'Dinosaur'};
	if(muni == 20770){name = 'Dolores'};
	if(muni == 21265){name = 'Dove Creek'};
	if(muni == 22035){name = 'Durango'};
	if(muni == 22145){name = 'Eads'};
	if(muni == 22200){name = 'Eagle'};
	if(muni == 22860){name = 'Eaton'};
	if(muni == 23025){name = 'Eckley'};
	if(muni == 23135){name = 'Edgewater'};
	if(muni == 23740){name = 'Elizabeth'};
	if(muni == 24620){name = 'Empire'};
	if(muni == 24785){name = 'Englewood'};
	if(muni == 24950){name = 'Erie'};
	if(muni == 25115){name = 'Estes Park'};
	if(muni == 25280){name = 'Evans'};
	if(muni == 25610){name = 'Fairplay'};
	if(muni == 26270){name = 'Federal Heights'};
	if(muni == 26600){name = 'Firestone'};
	if(muni == 26765){name = 'Flagler'};
	if(muni == 26875){name = 'Fleming'};
	if(muni == 27040){name = 'Florence'};
	if(muni == 27425){name = 'Fort Collins'};
	if(muni == 27700){name = 'Fort Lupton'};
	if(muni == 27810){name = 'Fort Morgan'};
	if(muni == 27865){name = 'Fountain'};
	if(muni == 27975){name = 'Fowler'};
	if(muni == 28105){name = 'Foxfield'};
	if(muni == 28305){name = 'Fraser'};
	if(muni == 28360){name = 'Frederick'};
	if(muni == 28690){name = 'Frisco'};
	if(muni == 28745){name = 'Fruita'};
	if(muni == 29185){name = 'Garden City'};
	if(muni == 29680){name = 'Genoa'};
	if(muni == 29735){name = 'Georgetown'};
	if(muni == 29955){name = 'Gilcrest'};
	if(muni == 30340){name = 'Glendale'};
	if(muni == 30780){name = 'Glenwood Springs'};
	if(muni == 30835){name = 'Golden'};
	if(muni == 31550){name = 'Granada'};
	if(muni == 31605){name = 'Granby'};
	if(muni == 31660){name = 'Grand Junction'};
	if(muni == 31715){name = 'Grand Lake'};
	if(muni == 32155){name = 'Greeley'};
	if(muni == 32650){name = 'Green Mountain Falls'};
	if(muni == 33035){name = 'Greenwood Village'};
	if(muni == 33310){name = 'Grover'};
	if(muni == 33640){name = 'Gunnison'};
	if(muni == 33695){name = 'Gypsum'};
	if(muni == 34520){name = 'Hartman'};
	if(muni == 34740){name = 'Haswell'};
	if(muni == 34960){name = 'Haxtun'};
	if(muni == 35070){name = 'Hayden'};
	if(muni == 36610){name = 'Hillrose'};
	if(muni == 37215){name = 'Holly'};
	if(muni == 37270){name = 'Holyoke'};
	if(muni == 37380){name = 'Hooper'};
	if(muni == 37545){name = 'Hotchkiss'};
	if(muni == 37600){name = 'Hot Sulphur Springs'};
	if(muni == 37820){name = 'Hudson'};
	if(muni == 37875){name = 'Hugo'};
	if(muni == 38370){name = 'Idaho Springs'};
	if(muni == 38535){name = 'Ignacio'};
	if(muni == 38590){name = 'Iliff'};
	if(muni == 39195){name = 'Jamestown'};
	if(muni == 39855){name = 'Johnstown'};
	if(muni == 39965){name = 'Julesburg'};
	if(muni == 40185){name = 'Keenesburg'};
	if(muni == 40515){name = 'Kersey'};
	if(muni == 40570){name = 'Kim'};
	if(muni == 40790){name = 'Kiowa'};
	if(muni == 41010){name = 'Kit Carson'};
	if(muni == 41560){name = 'Kremmling'};
	if(muni == 41835){name = 'Lafayette'};
	if(muni == 42055){name = 'La Jara'};
	if(muni == 42110){name = 'La Junta'};
	if(muni == 42330){name = 'Lake City'};
	if(muni == 42495){name = 'Lakeside'};
	if(muni == 43000){name = 'Lakewood'};
	if(muni == 43110){name = 'Lamar'};
	if(muni == 43550){name = 'Larkspur'};
	if(muni == 43605){name = 'La Salle'};
	if(muni == 43660){name = 'Las Animas'};
	if(muni == 44100){name = 'La Veta'};
	if(muni == 44320){name = 'Leadville'};
	if(muni == 44980){name = 'Limon'};
	if(muni == 45255){name = 'Littleton'};
	if(muni == 45530){name = 'Lochbuie'};
	if(muni == 45695){name = 'Log Lane Village'};
	if(muni == 45955){name = 'Lone Tree'};
	if(muni == 45970){name = 'Longmont'};
	if(muni == 46355){name = 'Louisville'};
	if(muni == 46465){name = 'Loveland'};
	if(muni == 47070){name = 'Lyons'};
	if(muni == 48060){name = 'Manassa'};
	if(muni == 48115){name = 'Mancos'};
	if(muni == 48445){name = 'Manitou Springs'};
	if(muni == 48500){name = 'Manzanola'};
	if(muni == 48555){name = 'Marble'};
	if(muni == 49600){name = 'Mead'};
	if(muni == 49875){name = 'Meeker'};
	if(muni == 50040){name = 'Merino'};
	if(muni == 50480){name = 'Milliken'};
	if(muni == 50920){name = 'Minturn'};
	if(muni == 51250){name = 'Moffat'};
	if(muni == 51635){name = 'Monte Vista'};
	if(muni == 51690){name = 'Montezuma'};
	if(muni == 51745){name = 'Montrose'};
	if(muni == 51800){name = 'Monument'};
	if(muni == 52075){name = 'Morrison'};
	if(muni == 52350){name = 'Mountain View'};
	if(muni == 52550){name = 'Mountain Village'};
	if(muni == 52570){name = 'Mount Crested Butte'};
	if(muni == 53120){name = 'Naturita'};
	if(muni == 53175){name = 'Nederland'};
	if(muni == 53395){name = 'New Castle'};
	if(muni == 54330){name = 'Northglenn'};
	if(muni == 54880){name = 'Norwood'};
	if(muni == 54935){name = 'Nucla'};
	if(muni == 55045){name = 'Nunn'};
	if(muni == 55155){name = 'Oak Creek'};
	if(muni == 55540){name = 'Olathe'};
	if(muni == 55705){name = 'Olney Springs'};
	if(muni == 55870){name = 'Ophir'};
	if(muni == 55980){name = 'Orchard City'};
	if(muni == 56145){name = 'Ordway'};
	if(muni == 56365){name = 'Otis'};
	if(muni == 56420){name = 'Ouray'};
	if(muni == 56475){name = 'Ovid'};
	if(muni == 56860){name = 'Pagosa Springs'};
	if(muni == 56970){name = 'Palisade'};
	if(muni == 57025){name = 'Palmer Lake'};
	if(muni == 57245){name = 'Paoli'};
	if(muni == 57300){name = 'Paonia'};
	if(muni == 57400){name = 'Parachute'};
	if(muni == 57630){name = 'Parker'};
	if(muni == 58235){name = 'Peetz'};
	if(muni == 59005){name = 'Pierce'};
	if(muni == 59830){name = 'Pitkin'};
	if(muni == 60160){name = 'Platteville'};
	if(muni == 60600){name = 'Poncha Springs'};
	if(muni == 61315){name = 'Pritchett'};
	if(muni == 62000){name = 'Pueblo'};
	if(muni == 62660){name = 'Ramah'};
	if(muni == 62880){name = 'Rangely'};
	if(muni == 63045){name = 'Raymer (New Raymer)'};
	if(muni == 63265){name = 'Red Cliff'};
	if(muni == 64090){name = 'Rico'};
	if(muni == 64200){name = 'Ridgway'};
	if(muni == 64255){name = 'Rifle'};
	if(muni == 64970){name = 'Rockvale'};
	if(muni == 65190){name = 'Rocky Ford'};
	if(muni == 65740){name = 'Romeo'};
	if(muni == 66895){name = 'Rye'};
	if(muni == 67005){name = 'Saguache'};
	if(muni == 67280){name = 'Salida'};
	if(muni == 67830){name = 'Sanford'};
	if(muni == 68105){name = 'San Luis'};
	if(muni == 68655){name = 'Sawpit'};
	if(muni == 68930){name = 'Sedgwick'};
	if(muni == 69040){name = 'Seibert'};
	if(muni == 69150){name = 'Severance'};
	if(muni == 69645){name = 'Sheridan'};
	if(muni == 69700){name = 'Sheridan Lake'};
	if(muni == 70195){name = 'Silt'};
	if(muni == 70250){name = 'Silver Cliff'};
	if(muni == 70360){name = 'Silver Plume'};
	if(muni == 70525){name = 'Silverthorne'};
	if(muni == 70580){name = 'Silverton'};
	if(muni == 70635){name = 'Simla'};
	if(muni == 71755){name = 'Snowmass Village'};
	if(muni == 72395){name = 'South Fork'};
	if(muni == 73330){name = 'Springfield'};
	if(muni == 73715){name = 'Starkville'};
	if(muni == 73825){name = 'Steamboat Springs'};
	if(muni == 73935){name = 'Sterling'};
	if(muni == 74485){name = 'Stratton'};
	if(muni == 74815){name = 'Sugar City'};
	if(muni == 75640){name = 'Superior'};
	if(muni == 75970){name = 'Swink'};
	if(muni == 76795){name = 'Telluride'};
	if(muni == 77290){name = 'Thornton'};
	if(muni == 77510){name = 'Timnath'};
	if(muni == 78610){name = 'Trinidad'};
	if(muni == 79270){name = 'Two Buttes'};
	if(muni == 80040){name = 'Vail'};
	if(muni == 80865){name = 'Victor'};
	if(muni == 81030){name = 'Vilas'};
	if(muni == 81690){name = 'Vona'};
	if(muni == 82130){name = 'Walden'};
	if(muni == 82350){name = 'Walsenburg'};
	if(muni == 82460){name = 'Walsh'};
	if(muni == 82735){name = 'Ward'};
	if(muni == 83230){name = 'Wellington'};
	if(muni == 83450){name = 'Westcliffe'};
	if(muni == 83835){name = 'Westminster'};
	if(muni == 84440){name = 'Wheat Ridge'};
	if(muni == 84770){name = 'Wiggins'};
	if(muni == 85045){name = 'Wiley'};
	if(muni == 85155){name = 'Williamsburg'};
	if(muni == 85485){name = 'Windsor'};
	if(muni == 85705){name = 'Winter Park'};
	if(muni == 86090){name = 'Woodland Park'};
	if(muni == 86310){name = 'Wray'};
	if(muni == 86475){name = 'Yampa'};
	if(muni == 86750){name = 'Yuma'};
return name;
};  
// muniName


function ctyNum(name) {
//ctyNum returns numeric county fips code from county Name
	var num;
	if(name == 'Colorado'){num = 0};
	if(name == 'Adams County'){num = 1};
	if(name == 'Alamosa County'){num = 3};
	if(name == 'Arapahoe County'){num = 5};
	if(name == 'Archuleta County'){num = 7};
	if(name == 'Baca County'){num = 9};
	if(name == 'Bent County'){num = 11};
	if(name == 'Boulder County'){num = 13};
	if(name == 'Broomfield County'){num = 14};
	if(name == 'Chaffee County'){num = 15};
	if(name == 'Cheyenne County'){num = 17};
	if(name == 'Clear Creek County'){num = 19};
	if(name == 'Conejos County'){num = 21};
	if(name == 'Costilla County'){num = 23};
	if(name == 'Crowley County'){num = 25};
	if(name == 'Custer County'){num = 27};
	if(name == 'Delta County'){num = 29};
	if(name == 'Denver County'){num = 31};
	if(name == 'Dolores County'){num = 33};
	if(name == 'Douglas County'){num = 35};
	if(name == 'Eagle County'){num = 37};
	if(name == 'Elbert County'){num = 39};
	if(name == 'El Paso County'){num = 41};
	if(name == 'Fremont County'){num = 43};
	if(name == 'Garfield County'){num = 45};
	if(name == 'Gilpin County'){num = 47};
	if(name == 'Grand County'){num = 49};
	if(name == 'Gunnison County'){num = 51};
	if(name == 'Hinsdale County'){num = 53};
	if(name == 'Huerfano County'){num = 55};
	if(name == 'Jackson County'){num = 57};
	if(name == 'Jefferson County'){num = 59};
	if(name == 'Kiowa County'){num = 61};
	if(name == 'Kit Carson County'){num = 63};
	if(name == 'Lake County'){num = 65};
	if(name == 'La Plata County'){num = 67};
	if(name == 'Larimer County'){num = 69};
	if(name == 'Las Animas County'){num = 71};
	if(name == 'Lincoln County'){num = 73};
	if(name == 'Logan County'){num = 75};
	if(name == 'Mesa County'){num = 77};
	if(name == 'Mineral County'){num = 79};
	if(name == 'Moffat County'){num = 81};
	if(name == 'Montezuma County'){num = 83};
	if(name == 'Montrose County'){num = 85};
	if(name == 'Morgan County'){num = 87};
	if(name == 'Otero County'){num = 89};
	if(name == 'Ouray County'){num = 91};
	if(name == 'Park County'){num = 93};
	if(name == 'Phillips County'){num = 95};
	if(name == 'Pitkin County'){num = 97};
	if(name == 'Prowers County'){num = 99};
	if(name == 'Pueblo County'){num = 101};
	if(name == 'Rio Blanco County'){num = 103};
	if(name == 'Rio Grande County'){num = 105};
	if(name == 'Routt County'){num = 107};
	if(name == 'Saguache County'){num = 109};
	if(name == 'San Luis County'){num = 111};
	if(name == 'San Miguel County'){num = 113};
	if(name == 'Sedgwick County'){num = 115};
	if(name == 'Summit County'){num = 117};
	if(name == 'Teller County'){num = 119};
	if(name == 'Washington County'){num = 121};
	if(name == 'Weld County'){num = 123};
	if(name == 'Yuma County'){num = 125};
	return(num)
} 
// ctyNum


function muniNum(name) {
//muniNum returns numeric municipality fips code from municipality Name
	var num;
	if(name == 'Colorado'){num = 0};
	if(name == 'Aguilar'){num = 760};
	if(name == 'Akron'){num = 925};
	if(name == 'Alamosa'){num = 1090};
	if(name == 'Alma'){num = 1530};
	if(name == 'Antonito'){num = 2355};
	if(name == 'Arriba'){num = 3235};
	if(name == 'Arvada'){num = 3455};
	if(name == 'Aspen'){num = 3620};
	if(name == 'Ault'){num = 3950};
	if(name == 'Aurora'){num = 4000};
	if(name == 'Avon'){num = 4110};
	if(name == 'Basalt'){num = 4935};
	if(name == 'Bayfield'){num = 5265};
	if(name == 'Bennett'){num = 6090};
	if(name == 'Berthoud'){num = 6255};
	if(name == 'Bethune'){num = 6530};
	if(name == 'Black Hawk'){num = 7025};
	if(name == 'Blanca'){num = 7190};
	if(name == 'Blue River'){num = 7410};
	if(name == 'Bonanza'){num = 7571};
	if(name == 'Boone'){num = 7795};
	if(name == 'Boulder'){num = 7850};
	if(name == 'Bow Mar'){num = 8070};
	if(name == 'Branson'){num = 8345};
	if(name == 'Breckenridge'){num = 8400};
	if(name == 'Brighton'){num = 8675};
	if(name == 'Brookside'){num = 9115};
	if(name == 'Broomfield'){num = 9280};
	if(name == 'Brush'){num = 9555};
	if(name == 'Buena Vista'){num = 10105};
	if(name == 'Burlington'){num = 10600};
	if(name == 'Calhan'){num = 11260};
	if(name == 'Campo'){num = 11645};
	if(name == 'Cañon City'){num = 11810};
	if(name == 'Carbonate'){num = 12030};
	if(name == 'Carbondale'){num = 12045};
	if(name == 'Castle Pines'){num = 12387};
	if(name == 'Castle Rock'){num = 12415};
	if(name == 'Cedaredge'){num = 12635};
	if(name == 'Centennial'){num = 12815};
	if(name == 'Center'){num = 12855};
	if(name == 'Central City'){num = 12910};
	if(name == 'Cheraw'){num = 13460};
	if(name == 'Cherry Hills Village'){num = 13845};
	if(name == 'Cheyenne Wells'){num = 14175};
	if(name == 'City of Creede'){num = 14765};
	if(name == 'Coal Creek'){num = 15330};
	if(name == 'Cokedale'){num = 15550};
	if(name == 'Collbran'){num = 15605};
	if(name == 'Colorado Springs'){num = 16000};
	if(name == 'Columbine Valley'){num = 16385};
	if(name == 'Commerce City'){num = 16495};
	if(name == 'Cortez'){num = 17375};
	if(name == 'Craig'){num = 17760};
	if(name == 'Crawford'){num = 17925};
	if(name == 'Crested Butte'){num = 18310};
	if(name == 'Crestone'){num = 18420};
	if(name == 'Cripple Creek'){num = 18530};
	if(name == 'Crook'){num = 18640};
	if(name == 'Crowley'){num = 18750};
	if(name == 'Dacono'){num = 19080};
	if(name == 'De Beque'){num = 19355};
	if(name == 'Deer Trail'){num = 19630};
	if(name == 'Del Norte'){num = 19795};
	if(name == 'Delta'){num = 19850};
	if(name == 'Denver'){num = 20000};
	if(name == 'Dillon'){num = 20440};
	if(name == 'Dinosaur'){num = 20495};
	if(name == 'Dolores'){num = 20770};
	if(name == 'Dove Creek'){num = 21265};
	if(name == 'Durango'){num = 22035};
	if(name == 'Eads'){num = 22145};
	if(name == 'Eagle'){num = 22200};
	if(name == 'Eaton'){num = 22860};
	if(name == 'Eckley'){num = 23025};
	if(name == 'Edgewater'){num = 23135};
	if(name == 'Elizabeth'){num = 23740};
	if(name == 'Empire'){num = 24620};
	if(name == 'Englewood'){num = 24785};
	if(name == 'Erie'){num = 24950};
	if(name == 'Estes Park'){num = 25115};
	if(name == 'Evans'){num = 25280};
	if(name == 'Fairplay'){num = 25610};
	if(name == 'Federal Heights'){num = 26270};
	if(name == 'Firestone'){num = 26600};
	if(name == 'Flagler'){num = 26765};
	if(name == 'Fleming'){num = 26875};
	if(name == 'Florence'){num = 27040};
	if(name == 'Fort Collins'){num = 27425};
	if(name == 'Fort Lupton'){num = 27700};
	if(name == 'Fort Morgan'){num = 27810};
	if(name == 'Fountain'){num = 27865};
	if(name == 'Fowler'){num = 27975};
	if(name == 'Foxfield'){num = 28105};
	if(name == 'Fraser'){num = 28305};
	if(name == 'Frederick'){num = 28360};
	if(name == 'Frisco'){num = 28690};
	if(name == 'Fruita'){num = 28745};
	if(name == 'Garden City'){num = 29185};
	if(name == 'Genoa'){num = 29680};
	if(name == 'Georgetown'){num = 29735};
	if(name == 'Gilcrest'){num = 29955};
	if(name == 'Glendale'){num = 30340};
	if(name == 'Glenwood Springs'){num = 30780};
	if(name == 'Golden'){num = 30835};
	if(name == 'Granada'){num = 31550};
	if(name == 'Granby'){num = 31605};
	if(name == 'Grand Junction'){num = 31660};
	if(name == 'Grand Lake'){num = 31715};
	if(name == 'Greeley'){num = 32155};
	if(name == 'Green Mountain Falls'){num = 32650};
	if(name == 'Greenwood Village'){num = 33035};
	if(name == 'Grover'){num = 33310};
	if(name == 'Gunnison'){num = 33640};
	if(name == 'Gypsum'){num = 33695};
	if(name == 'Hartman'){num = 34520};
	if(name == 'Haswell'){num = 34740};
	if(name == 'Haxtun'){num = 34960};
	if(name == 'Hayden'){num = 35070};
	if(name == 'Hillrose'){num = 36610};
	if(name == 'Holly'){num = 37215};
	if(name == 'Holyoke'){num = 37270};
	if(name == 'Hooper'){num = 37380};
	if(name == 'Hotchkiss'){num = 37545};
	if(name == 'Hot Sulphur Springs'){num = 37600};
	if(name == 'Hudson'){num = 37820};
	if(name == 'Hugo'){num = 37875};
	if(name == 'Idaho Springs'){num = 38370};
	if(name == 'Ignacio'){num = 38535};
	if(name == 'Iliff'){num = 38590};
	if(name == 'Jamestown'){num = 39195};
	if(name == 'Johnstown'){num = 39855};
	if(name == 'Julesburg'){num = 39965};
	if(name == 'Keenesburg'){num = 40185};
	if(name == 'Kersey'){num = 40515};
	if(name == 'Kim'){num = 40570};
	if(name == 'Kiowa'){num = 40790};
	if(name == 'Kit Carson'){num = 41010};
	if(name == 'Kremmling'){num = 41560};
	if(name == 'Lafayette'){num = 41835};
	if(name == 'La Jara'){num = 42055};
	if(name == 'La Junta'){num = 42110};
	if(name == 'Lake City'){num = 42330};
	if(name == 'Lakeside'){num = 42495};
	if(name == 'Lakewood'){num = 43000};
	if(name == 'Lamar'){num = 43110};
	if(name == 'Larkspur'){num = 43550};
	if(name == 'La Salle'){num = 43605};
	if(name == 'Las Animas'){num = 43660};
	if(name == 'La Veta'){num = 44100};
	if(name == 'Leadville'){num = 44320};
	if(name == 'Limon'){num = 44980};
	if(name == 'Littleton'){num = 45255};
	if(name == 'Lochbuie'){num = 45530};
	if(name == 'Log Lane Village'){num = 45695};
	if(name == 'Lone Tree'){num = 45955};
	if(name == 'Longmont'){num = 45970};
	if(name == 'Louisville'){num = 46355};
	if(name == 'Loveland'){num = 46465};
	if(name == 'Lyons'){num = 47070};
	if(name == 'Manassa'){num = 48060};
	if(name == 'Mancos'){num = 48115};
	if(name == 'Manitou Springs'){num = 48445};
	if(name == 'Manzanola'){num = 48500};
	if(name == 'Marble'){num = 48555};
	if(name == 'Mead'){num = 49600};
	if(name == 'Meeker'){num = 49875};
	if(name == 'Merino'){num = 50040};
	if(name == 'Milliken'){num = 50480};
	if(name == 'Minturn'){num = 50920};
	if(name == 'Moffat'){num = 51250};
	if(name == 'Monte Vista'){num = 51635};
	if(name == 'Montezuma'){num = 51690};
	if(name == 'Montrose'){num = 51745};
	if(name == 'Monument'){num = 51800};
	if(name == 'Morrison'){num = 52075};
	if(name == 'Mountain View'){num = 52350};
	if(name == 'Mountain Village'){num = 52550};
	if(name == 'Mount Crested Butte'){num = 52570};
	if(name == 'Naturita'){num = 53120};
	if(name == 'Nederland'){num = 53175};
	if(name == 'New Castle'){num = 53395};
	if(name == 'Northglenn'){num = 54330};
	if(name == 'Norwood'){num = 54880};
	if(name == 'Nucla'){num = 54935};
	if(name == 'Nunn'){num = 55045};
	if(name == 'Oak Creek'){num = 55155};
	if(name == 'Olathe'){num = 55540};
	if(name == 'Olney Springs'){num = 55705};
	if(name == 'Ophir'){num = 55870};
	if(name == 'Orchard City'){num = 55980};
	if(name == 'Ordway'){num = 56145};
	if(name == 'Otis'){num = 56365};
	if(name == 'Ouray'){num = 56420};
	if(name == 'Ovid'){num = 56475};
	if(name == 'Pagosa Springs'){num = 56860};
	if(name == 'Palisade'){num = 56970};
	if(name == 'Palmer Lake'){num = 57025};
	if(name == 'Paoli'){num = 57245};
	if(name == 'Paonia'){num = 57300};
	if(name == 'Parachute'){num = 57400};
	if(name == 'Parker'){num = 57630};
	if(name == 'Peetz'){num = 58235};
	if(name == 'Pierce'){num = 59005};
	if(name == 'Pitkin'){num = 59830};
	if(name == 'Platteville'){num = 60160};
	if(name == 'Poncha Springs'){num = 60600};
	if(name == 'Pritchett'){num = 61315};
	if(name == 'Pueblo'){num = 62000};
	if(name == 'Ramah'){num = 62660};
	if(name == 'Rangely'){num = 62880};
	if(name == 'Raymer (New Raymer'){num = 63045};
	if(name == 'Red Cliff'){num = 63265};
	if(name == 'Rico'){num = 64090};
	if(name == 'Ridgway'){num = 64200};
	if(name == 'Rifle'){num = 64255};
	if(name == 'Rockvale'){num = 64970};
	if(name == 'Rocky Ford'){num = 65190};
	if(name == 'Romeo'){num = 65740};
	if(name == 'Rye'){num = 66895};
	if(name == 'Saguache'){num = 67005};
	if(name == 'Salida'){num = 67280};
	if(name == 'Sanford'){num = 67830};
	if(name == 'San Luis'){num = 68105};
	if(name == 'Sawpit'){num = 68655};
	if(name == 'Sedgwick'){num = 68930};
	if(name == 'Seibert'){num = 69040};
	if(name == 'Severance'){num = 69150};
	if(name == 'Sheridan'){num = 69645};
	if(name == 'Sheridan Lake'){num = 69700};
	if(name == 'Silt'){num = 70195};
	if(name == 'Silver Cliff'){num = 70250};
	if(name == 'Silver Plume'){num = 70360};
	if(name == 'Silverthorne'){num = 70525};
	if(name == 'Silverton'){num = 70580};
	if(name == 'Simla'){num = 70635};
	if(name == 'Snowmass Village'){num = 71755};
	if(name == 'South Fork'){num = 72395};
	if(name == 'Springfield'){num = 73330};
	if(name == 'Starkville'){num = 73715};
	if(name == 'Steamboat Springs'){num = 73825};
	if(name == 'Sterling'){num = 73935};
	if(name == 'Stratton'){num = 74485};
	if(name == 'Sugar City'){num = 74815};
	if(name == 'Superior'){num = 75640};
	if(name == 'Swink'){num = 75970};
	if(name == 'Telluride'){num = 76795};
	if(name == 'Thornton'){num = 77290};
	if(name == 'Timnath'){num = 77510};
	if(name == 'Trinidad'){num = 78610};
	if(name == 'Two Buttes'){num = 79270};
	if(name == 'Vail'){num = 80040};
	if(name == 'Victor'){num = 80865};
	if(name == 'Vilas'){num = 81030};
	if(name == 'Vona'){num = 81690};
	if(name == 'Walden'){num = 82130};
	if(name == 'Walsenburg'){num = 82350};
	if(name == 'Walsh'){num = 82460};
	if(name == 'Ward'){num = 82735};
	if(name == 'Wellington'){num = 83230};
	if(name == 'Westcliffe'){num = 83450};
	if(name == 'Westminster'){num = 83835};
	if(name == 'Wheat Ridge'){num = 84440};
	if(name == 'Wiggins'){num = 84770};
	if(name == 'Wiley'){num = 85045};
	if(name == 'Williamsburg'){num = 85155};
	if(name == 'Windsor'){num = 85485};
	if(name == 'Winter Park'){num = 85705};
	if(name == 'Woodland Park'){num = 86090};
	if(name == 'Wray'){num = 86310};
	if(name == 'Yampa'){num = 86475};
	if(name == 'Yuma'){num = 86750};
	return(num)
} 
// muniNum


function cdpName(cdp){
//cdpName returns CDP Name from the numeric cdp fips code
	var name;
	if(cdp == 0) {name = 'Colorado'};
	if(cdp == 320){name = 'Acres Green CDP'};
	if(cdp == 620){name = 'Aetna Estates CDP'};
	if(cdp == 870){name = 'Air Force Academy CDP'};
	if(cdp == 1145){name = 'Alamosa East CDP'};
	if(cdp == 1420){name = 'Allenspark CDP'};
	if(cdp == 1640){name = 'Alpine CDP'};
	if(cdp == 1740){name = 'Altona CDP'};
	if(cdp == 1915){name = 'Amherst CDP'};
	if(cdp == 2575){name = 'Applewood CDP'};
	if(cdp == 2905){name = 'Arboles CDP'};
	if(cdp == 3015){name = 'Aristocrat Ranchettes CDP'};
	if(cdp == 3730){name = 'Aspen Park CDP'};
	if(cdp == 3840){name = 'Atwood CDP'};
	if(cdp == 4165){name = 'Avondale CDP'};
	if(cdp == 4620){name = 'Bark Ranch CDP'};
	if(cdp == 5120){name = 'Battlement Mesa CDP'};
	if(cdp == 6172){name = 'Berkley CDP'};
	if(cdp == 6602){name = 'Beulah Valley CDP'};
	if(cdp == 6970){name = 'Black Forest CDP'};
	if(cdp == 7245){name = 'Blende CDP'};
	if(cdp == 7420){name = 'Blue Sky CDP'};
	if(cdp == 7580){name = 'Bonanza Mountain Estates CDP'};
	if(cdp == 8290){name = 'Brandon CDP'};
	if(cdp == 8530){name = 'Brick Center CDP'};
	if(cdp == 10985){name = 'Byers CDP'};
	if(cdp == 11975){name = 'Capulin CDP'};
	if(cdp == 12325){name = 'Cascade-Chipita Park CDP'};
	if(cdp == 12387){name = 'Castle Pines CDP'};
	if(cdp == 12450){name = 'Cathedral CDP'};
	if(cdp == 12460){name = 'Catherine CDP'};
	if(cdp == 12470){name = 'Cattle Creek CDP'};
	if(cdp == 12945){name = 'Chacra CDP'};
	if(cdp == 13590){name = 'Cherry Creek CDP'};
	if(cdp == 14587){name = 'Cimarron Hills CDP'};
	if(cdp == 15165){name = 'Clifton CDP'};
	if(cdp == 15302){name = 'Coal Creek CDP'};
	if(cdp == 15440){name = 'Coaldale CDP'};
	if(cdp == 15825){name = 'Colona CDP'};
	if(cdp == 15935){name = 'Colorado City CDP'};
	if(cdp == 16110){name = 'Columbine CDP'};
	if(cdp == 16465){name = 'Comanche Creek CDP'};
	if(cdp == 16715){name = 'Conejos CDP'};
	if(cdp == 17150){name = 'Copper Mountain CDP'};
	if(cdp == 17485){name = 'Cotopaxi CDP'};
	if(cdp == 18585){name = 'Crisman CDP'};
	if(cdp == 19150){name = 'Dakota Ridge CDP'};
	if(cdp == 20275){name = 'Derby CDP'};
	if(cdp == 20605){name = 'Divide CDP'};
	if(cdp == 21155){name = 'Dotsero CDP'};
	if(cdp == 21330){name = 'Dove Valley CDP'};
	if(cdp == 21390){name = 'Downieville-Lawson-Dumont CDP'};
	if(cdp == 22575){name = 'East Pleasant View CDP'};
	if(cdp == 23300){name = 'Edwards CDP'};
	if(cdp == 23520){name = 'Elbert CDP'};
	if(cdp == 23575){name = 'Eldora CDP'};
	if(cdp == 23630){name = 'Eldorado Springs CDP'};
	if(cdp == 23795){name = 'El Jebel CDP'};
	if(cdp == 24235){name = 'Ellicott CDP'};
	if(cdp == 24290){name = 'El Moro CDP'};
	if(cdp == 25390){name = 'Evergreen CDP'};
	if(cdp == 25550){name = 'Fairmount CDP'};
	if(cdp == 27095){name = 'Florissant CDP'};
	if(cdp == 27175){name = 'Floyd Hill CDP'};
	if(cdp == 27370){name = 'Fort Carson CDP'};
	if(cdp == 27535){name = 'Fort Garland CDP'};
	if(cdp == 28250){name = 'Franktown CDP'};
	if(cdp == 28800){name = 'Fruitvale CDP'};
	if(cdp == 28830){name = 'Fulford CDP'};
	if(cdp == 29295){name = 'Garfield CDP'};
	if(cdp == 29625){name = 'Genesee CDP'};
	if(cdp == 29845){name = 'Gerrard CDP'};
	if(cdp == 30350){name = 'Glendale CDP'};
	if(cdp == 30420){name = 'Gleneagle CDP'};
	if(cdp == 30890){name = 'Goldfield CDP'};
	if(cdp == 30945){name = 'Gold Hill CDP'};
	if(cdp == 31935){name = 'Grand View Estates CDP'};
	if(cdp == 33420){name = 'Guffey CDP'};
	if(cdp == 33502){name = 'Gunbarrel CDP'};
	if(cdp == 34685){name = 'Hasty CDP'};
	if(cdp == 35400){name = 'Heeney CDP'};
	if(cdp == 35860){name = 'Hidden Lake CDP'};
	if(cdp == 36410){name = 'Highlands Ranch CDP'};
	if(cdp == 36940){name = 'Hoehne CDP'};
	if(cdp == 37220){name = 'Holly Hills CDP'};
	if(cdp == 37655){name = 'Howard CDP'};
	if(cdp == 38425){name = 'Idalia CDP'};
	if(cdp == 38480){name = 'Idledale CDP'};
	if(cdp == 38810){name = 'Indian Hills CDP'};
	if(cdp == 38910){name = 'Inverness CDP'};
	if(cdp == 39160){name = 'Jackson Lake CDP'};
	if(cdp == 39250){name = 'Jansen CDP'};
	if(cdp == 39745){name = 'Joes CDP'};
	if(cdp == 39800){name = 'Johnson Village CDP'};
	if(cdp == 40377){name = 'Ken Caryl CDP'};
	if(cdp == 40550){name = 'Keystone CDP'};
	if(cdp == 40900){name = 'Kirk CDP'};
	if(cdp == 41065){name = 'Kittredge CDP'};
	if(cdp == 42000){name = 'Laird CDP'};
	if(cdp == 42165){name = 'La Junta Gardens CDP'};
	if(cdp == 43220){name = 'Laporte CDP'};
	if(cdp == 44270){name = 'Lazy Acres CDP'};
	if(cdp == 44375){name = 'Leadville North CDP'};
	if(cdp == 44595){name = 'Lewis CDP'};
	if(cdp == 44695){name = 'Leyner CDP'};
	if(cdp == 45145){name = 'Lincoln Park CDP'};
	if(cdp == 45680){name = 'Loghill Village CDP'};
	if(cdp == 45750){name = 'Loma CDP'};
	if(cdp == 46410){name = 'Louviers CDP'};
	if(cdp == 47015){name = 'Lynn CDP'};
	if(cdp == 47345){name = 'McCoy CDP'};
	if(cdp == 49325){name = 'Maybell CDP'};
	if(cdp == 49490){name = 'Maysville CDP'};
	if(cdp == 50012){name = 'Meridian CDP'};
	if(cdp == 50380){name = 'Midland CDP'};
	if(cdp == 51975){name = 'Morgan Heights CDP'};
	if(cdp == 52210){name = 'Mountain Meadows CDP'};
	if(cdp == 52820){name = 'Mulford CDP'};
	if(cdp == 53780){name = 'Niwot CDP'};
	if(cdp == 53875){name = 'No Name CDP'};
	if(cdp == 53945){name = 'Norrie CDP'};
	if(cdp == 54495){name = 'North La Junta CDP'};
	if(cdp == 54750){name = 'North Washington CDP'};
	if(cdp == 55925){name = 'Orchard CDP'};
	if(cdp == 56035){name = 'Orchard Mesa CDP'};
	if(cdp == 56695){name = 'Padroni CDP'};
	if(cdp == 57445){name = 'Paragon Estates CDP'};
	if(cdp == 57850){name = 'Parshall CDP'};
	if(cdp == 58400){name = 'Penrose CDP'};
	if(cdp == 58510){name = 'Peoria CDP'};
	if(cdp == 58592){name = 'Perry Park CDP'};
	if(cdp == 58675){name = 'Peyton CDP'};
	if(cdp == 58758){name = 'Phippsburg CDP'};
	if(cdp == 58960){name = 'Piedra CDP'};
	if(cdp == 59240){name = 'Pine Brook Hill CDP'};
	if(cdp == 60655){name = 'Ponderosa Park CDP'};
	if(cdp == 60765){name = 'Portland CDP'};
	if(cdp == 62220){name = 'Pueblo West CDP'};
	if(cdp == 63320){name = 'Red Feather Lakes CDP'};
	if(cdp == 63375){name = 'Redlands CDP'};
	if(cdp == 63650){name = 'Redstone CDP'};
	if(cdp == 63705){name = 'Redvale CDP'};
	if(cdp == 64870){name = 'Rock Creek Park CDP'};
	if(cdp == 65685){name = 'Rollinsville CDP'};
	if(cdp == 66197){name = 'Roxborough Park CDP'};
	if(cdp == 66995){name = 'Saddle Ridge CDP'};
	if(cdp == 67040){name = 'St. Ann Highlands CDP'};
	if(cdp == 67142){name = "St. Mary's CDP"};
	if(cdp == 67445){name = 'Salt Creek CDP'};
	if(cdp == 67500){name = 'San Acacio CDP'};
	if(cdp == 68847){name = 'Security-Widefield CDP'};
	if(cdp == 68875){name = 'Sedalia CDP'};
	if(cdp == 68985){name = 'Segundo CDP'};
	if(cdp == 69110){name = 'Seven Hills CDP'};
	if(cdp == 69480){name = 'Shaw Heights CDP'};
	if(cdp == 69810){name = 'Sherrelwood CDP'};
	if(cdp == 71625){name = 'Smeltertown CDP'};
	if(cdp == 71790){name = 'Snyder CDP'};
	if(cdp == 72320){name = 'Southern Ute CDP'};
	if(cdp == 74080){name = 'Stonegate CDP'};
	if(cdp == 74275){name = 'Stonewall Gap CDP'};
	if(cdp == 74375){name = 'Strasburg CDP'};
	if(cdp == 74430){name = 'Stratmoor CDP'};
	if(cdp == 74980){name = 'Sugarloaf CDP'};
	if(cdp == 75585){name = 'Sunshine CDP'};
	if(cdp == 76190){name = 'Tabernash CDP'};
	if(cdp == 76325){name = 'Tall Timber CDP'};
	if(cdp == 77235){name = 'The Pinery CDP'};
	if(cdp == 77757){name = 'Todd Creek CDP'};
	if(cdp == 78280){name = 'Towaoc CDP'};
	if(cdp == 78335){name = 'Towner CDP'};
	if(cdp == 78345){name = 'Trail Side CDP'};
	if(cdp == 79100){name = 'Twin Lakes CDP'};
	if(cdp == 79105){name = 'Twin Lakes CDP'};
	if(cdp == 79785){name = 'Upper Bear Creek CDP'};
	if(cdp == 80095){name = 'Valdez CDP'};
	if(cdp == 80370){name = 'Valmont CDP'};
	if(cdp == 80755){name = 'Vernon CDP'};
	if(cdp == 81305){name = 'Vineland CDP'};
	if(cdp == 82905){name = 'Watkins CDP'};
	if(cdp == 83120){name = 'Welby CDP'};
	if(cdp == 83175){name = 'Weldona CDP'};
	if(cdp == 83500){name = 'Westcreek CDP'};
	if(cdp == 84000){name = 'Weston CDP'};
	if(cdp == 84042){name = 'West Pleasant View CDP'};
	if(cdp == 85760){name = 'Wolcott CDP'};
	if(cdp == 86117){name = 'Woodmoor CDP'};
	if(cdp == 86200){name = 'Woody Creek CDP'};
return name;
}; 
// cdpName


function popDropdown(level,ddid,callpg) {
//popDropdown populates drop down boxes based on input geography Type
   
   //Counties
var county = [  {'location':'Colorado', 'fips': '000'}, {'location':'Adams County', 'fips': '001'},
                {'location':'Alamosa County', 'fips': '003'},{'location':'Arapahoe County', 'fips': '005'},
				{'location':'Archuleta County', 'fips': '007'},{'location':'Baca County', 'fips': '009'},
				{'location':'Bent County', 'fips': '011'},{'location':'Boulder County', 'fips': '013'},
				{'location':'Broomfield County', 'fips': '014'},{'location':'Chaffee County', 'fips': '015'},
				{'location':'Cheyenne County', 'fips': '017'},{'location':'Clear Creek County', 'fips': '019'},
				{'location':'Conejos County', 'fips': '021'},{'location':'Costilla County', 'fips': '023'},
				{'location':'Crowley County', 'fips': '025'},{'location':'Custer County', 'fips': '027'},
				{'location':'Delta County', 'fips': '029'},{'location':'Denver County', 'fips': '031'},
				{'location':'Dolores County', 'fips': '033'},{'location':'Douglas County', 'fips': '035'},
				{'location':'Eagle County', 'fips': '037'},{'location':'Elbert County', 'fips': '039'},
				{'location':'El Paso County', 'fips': '041'},{'location':'Fremont County', 'fips': '043'},
				{'location':'Garfield County', 'fips': '045'},{'location':'Gilpin County', 'fips': '047'},
				{'location':'Grand County', 'fips': '049'},{'location':'Gunnison County', 'fips': '051'},
				{'location':'Hinsdale County', 'fips': '053'},{'location':'Huerfano County', 'fips': '055'},
				{'location':'Jackson County', 'fips': '057'},{'location':'Jefferson County', 'fips': '059'},
				{'location':'Kiowa County', 'fips': '061'},{'location':'Kit Carson County', 'fips': '063'},
				{'location':'Lake County', 'fips': '065'},{'location':'La Plata County', 'fips': '067'},
				{'location':'Larimer County', 'fips': '069'},{'location':'Las Animas County', 'fips': '071'},
				{'location':'Lincoln County', 'fips': '073'},{'location':'Logan County', 'fips': '075'},
				{'location':'Mesa County', 'fips': '077'},{'location':'Mineral County', 'fips': '079'},
				{'location':'Moffat County', 'fips': '081'},{'location':'Montezuma County', 'fips': '083'},
				{'location':'Montrose County', 'fips': '085'},{'location':'Morgan County', 'fips': '087'},
				{'location':'Otero County', 'fips': '089'},{'location':'Ouray County', 'fips': '091'},
				{'location':'Park County', 'fips': '093'},{'location':'Phillips County', 'fips': '095'},
				{'location':'Pitkin County', 'fips': '097'},{'location':'Prowers County', 'fips': '099'},
				{'location':'Pueblo County', 'fips': '101'},{'location':'Rio Blanco County', 'fips': '103'},
				{'location':'Rio Grande County', 'fips': '105'},{'location':'Routt County', 'fips': '107'},
				{'location':'Saguache County', 'fips': '109'},{'location':'San Luis County', 'fips': '111'},
				{'location':'San Miguel County', 'fips': '113'},{'location':'Sedgwick County', 'fips': '115'},
				{'location':'Summit County', 'fips': '117'},{'location':'Teller County', 'fips': '119'},
				{'location':'Washington County', 'fips': '121'},{'location':'Weld County', 'fips': '123'},
				{'location':'Yuma County', 'fips': '125'}];

if(callpg == "jobs"){
	var county = [{'location':'Colorado', 'fips': '000'}, {'location':'Denver-Boulder Metro Area', 'fips': '500'},
				{'location':'Adams County', 'fips': '001'},
                {'location':'Alamosa County', 'fips': '003'},{'location':'Arapahoe County', 'fips': '005'},
				{'location':'Archuleta County', 'fips': '007'},{'location':'Baca County', 'fips': '009'},
				{'location':'Bent County', 'fips': '011'},{'location':'Boulder County', 'fips': '013'},
				{'location':'Broomfield County', 'fips': '014'},
				{'location':'Chaffee County', 'fips': '015'},
				{'location':'Cheyenne County', 'fips': '017'},{'location':'Clear Creek County', 'fips': '019'},
				{'location':'Conejos County', 'fips': '021'},{'location':'Costilla County', 'fips': '023'},
				{'location':'Crowley County', 'fips': '025'},{'location':'Custer County', 'fips': '027'},
				{'location':'Delta County', 'fips': '029'},{'location':'Denver County', 'fips': '031'},
				{'location':'Dolores County', 'fips': '033'},{'location':'Douglas County', 'fips': '035'},
				{'location':'Eagle County', 'fips': '037'},{'location':'Elbert County', 'fips': '039'},
				{'location':'El Paso County', 'fips': '041'},{'location':'Fremont County', 'fips': '043'},
				{'location':'Garfield County', 'fips': '045'},{'location':'Gilpin County', 'fips': '047'},
				{'location':'Grand County', 'fips': '049'},{'location':'Gunnison County', 'fips': '051'},
				{'location':'Hinsdale County', 'fips': '053'},{'location':'Huerfano County', 'fips': '055'},
				{'location':'Jackson County', 'fips': '057'},{'location':'Jefferson County', 'fips': '059'},
				{'location':'Kiowa County', 'fips': '061'},{'location':'Kit Carson County', 'fips': '063'},
				{'location':'Lake County', 'fips': '065'},{'location':'La Plata County', 'fips': '067'},
				{'location':'Larimer County', 'fips': '069'},{'location':'Las Animas County', 'fips': '071'},
				{'location':'Lincoln County', 'fips': '073'},{'location':'Logan County', 'fips': '075'},
				{'location':'Mesa County', 'fips': '077'},{'location':'Mineral County', 'fips': '079'},
				{'location':'Moffat County', 'fips': '081'},{'location':'Montezuma County', 'fips': '083'},
				{'location':'Montrose County', 'fips': '085'},{'location':'Morgan County', 'fips': '087'},
				{'location':'Otero County', 'fips': '089'},{'location':'Ouray County', 'fips': '091'},
				{'location':'Park County', 'fips': '093'},{'location':'Phillips County', 'fips': '095'},
				{'location':'Pitkin County', 'fips': '097'},{'location':'Prowers County', 'fips': '099'},
				{'location':'Pueblo County', 'fips': '101'},{'location':'Rio Blanco County', 'fips': '103'},
				{'location':'Rio Grande County', 'fips': '105'},{'location':'Routt County', 'fips': '107'},
				{'location':'Saguache County', 'fips': '109'},{'location':'San Luis County', 'fips': '111'},
				{'location':'San Miguel County', 'fips': '113'},{'location':'Sedgwick County', 'fips': '115'},
				{'location':'Summit County', 'fips': '117'},{'location':'Teller County', 'fips': '119'},
				{'location':'Washington County', 'fips': '121'},{'location':'Weld County', 'fips': '123'},
				{'location':'Yuma County', 'fips': '125'}];
}
//regions

var region =  [
				{'optgroup' : 'Geographic Region','location' : 'Central Mountains', 'regnum' : '01'},	
				{'optgroup' : 'Geographic Region','location' : 'Eastern Plains', 'regnum' : '02'},
				{'optgroup' : 'Geographic Region','location' : 'Front Range', 'regnum' : '03'},
				{'optgroup' : 'Geographic Region','location' : 'San Luis Valley', 'regnum' : '04'},
				{'optgroup' : 'Geographic Region','location' : 'Western Slope', 'regnum' : '05'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 1: Northern Eastern Plains', 'regnum' : '06'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 2: Northern Front Range', 'regnum' : '07'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 3: Denver Metropolitan Area', 'regnum' : '08'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 4: Southern Front Range', 'regnum' : '09'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 5: Central Eastern Plains', 'regnum' : '10'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 6: Southern Eastern Plains', 'regnum' : '11'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 7: Pueblo County', 'regnum' : '12'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 8: San Luis Valley', 'regnum' : '13'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 9: Southern Western Slope', 'regnum' : '14'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 10: Central Western Slope', 'regnum' : '15'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 11: Northern Western Slope', 'regnum' : '16'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 12: Northern Mountains', 'regnum' : '17'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 13: Central Mountains', 'regnum' : '18'},
				{'optgroup' : 'Colorado Planning and Management Regions','location' : 'Region 14: Southern Mountains', 'regnum' : '19'},
				{'optgroup' : 'Census Metropolitan Statistical Areas', 'location' : 'Boulder', 'regnum' : '20'},
				{'optgroup' : 'Census Metropolitan Statistical Areas', 'location' : 'Colorado Springs', 'regnum' : '21'},
				{'optgroup' : 'Census Metropolitan Statistical Areas', 'location' : 'Denver-Aurora-Lakewood', 'regnum' : '22'},
				{'optgroup' : 'Census Metropolitan Statistical Areas', 'location' : 'Fort Collins', 'regnum' : '23'},
				{'optgroup' : 'Census Metropolitan Statistical Areas', 'location' : 'Grand Junction', 'regnum' : '24'},
				{'optgroup' : 'Census Metropolitan Statistical Areas', 'location' : 'Greeley', 'regnum' : '25'},
				{'optgroup' : 'Census Metropolitan Statistical Areas', 'location' : 'Pueblo', 'regnum' : '26'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Breckenridge', 'regnum' : '27'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Ca\u00f1on City', 'regnum' : '28'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Craig', 'regnum' : '29'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Durango', 'regnum' : '30'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Edwards', 'regnum' : '31'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Fort Morgan', 'regnum' : '32'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Glenwood Springs', 'regnum' : '33'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Montrose', 'regnum' : '34'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Steamboat Springs', 'regnum' : '35'},
				{'optgroup' : 'Census Micropolitan Statistical Areas', 'location' : 'Sterling', 'regnum' : '36'},
				{'optgroup' : 'Denver Regions','location' : 'Denver PMSA', 'regnum' : '37'},
				{'optgroup' : 'Denver Regions','location' : 'Denver-Boulder Metro Area', 'regnum' : '38'},
				{'optgroup' : 'Denver Regions','location' : 'Denver-Boulder-Greely CMSA', 'regnum' : '39'},
];

//Municipalities and places

var municipality = [{'location' :  'Aguilar' , 'fips' : '00760'}, {'location' :  'Akron' , 'fips' : '00925'},
		{'location' :  'Alamosa' , 'fips' : '01090'}, {'location' :  'Alma' , 'fips' : '01530'},
		{'location' :  'Antonito' , 'fips' : '02355'}, {'location' :  'Arriba' , 'fips' : '03235'},
		{'location' :  'Arvada' , 'fips' : '03455'}, {'location' :  'Aspen' , 'fips' : '03620'},
		{'location' :  'Ault' , 'fips' : '03950'}, {'location' :  'Aurora' , 'fips' : '04000'},
		{'location' :  'Avon' , 'fips' : '04110'}, {'location' :  'Basalt' , 'fips' : '04935'},
		{'location' :  'Bayfield' , 'fips' : '05265'}, {'location' :  'Bennett' , 'fips' : '06090'},
		{'location' :  'Berthoud' , 'fips' : '06255'}, {'location' :  'Bethune' , 'fips' : '06530'},
		{'location' :  'Black Hawk' , 'fips' : '07025'}, {'location' :  'Blanca' , 'fips' : '07190'},
		{'location' :  'Blue River' , 'fips' : '07410'}, {'location' :  'Bonanza' , 'fips' : '07571'},
		{'location' :  'Boone' , 'fips' : '07795'}, {'location' :  'Boulder' , 'fips' : '07850'},
		{'location' :  'Bow Mar' , 'fips' : '08070'}, {'location' :  'Branson' , 'fips' : '08345'},
		{'location' :  'Breckenridge' , 'fips' : '08400'}, {'location' :  'Brighton' , 'fips' : '08675'},
		{'location' :  'Brookside' , 'fips' : '09115'}, {'location' :  'Broomfield' , 'fips' : '09280'},
		{'location' :  'Brush' , 'fips' : '09555'}, {'location' :  'Buena Vista' , 'fips' : '10105'},
		{'location' :  'Burlington' , 'fips' : '10600'}, {'location' :  'Calhan' , 'fips' : '11260'},
		{'location' :  'Campo' , 'fips' : '11645'}, {'location' :  'Ca\u00f1on City' , 'fips' : '11810'},
		{'location' :  'Carbonate' , 'fips' : '12030'},{'location' :  'Carbondale' , 'fips' : '12045'}, {'location' :  'Castle Pines' , 'fips' : '12387'},
		{'location' :  'Castle Rock' , 'fips' : '12415'}, {'location' :  'Cedaredge' , 'fips' : '12635'},
		{'location' :  'Centennial' , 'fips' : '12815'}, {'location' :  'Center' , 'fips' : '12855'},
		{'location' :  'Central City' , 'fips' : '12910'}, {'location' :  'Cheraw' , 'fips' : '13460'},
		{'location' :  'Cherry Hills Village' , 'fips' : '13845'}, {'location' :  'Cheyenne Wells' , 'fips' : '14175'},
		{'location' :  'City of Creede' , 'fips' : '14765'}, {'location' :  'Coal Creek' , 'fips' : '15330'},
		{'location' :  'Cokedale' , 'fips' : '15550'}, {'location' :  'Collbran' , 'fips' : '15605'},
		{'location' :  'Colorado Springs' , 'fips' : '16000'}, {'location' :  'Columbine Valley' , 'fips' : '16385'},
		{'location' :  'Commerce City' , 'fips' : '16495'}, {'location' :  'Cortez' , 'fips' : '17375'},
		{'location' :  'Craig' , 'fips' : '17760'}, {'location' :  'Crawford' , 'fips' : '17925'},
		{'location' :  'Crested Butte' , 'fips' : '18310'}, {'location' :  'Crestone' , 'fips' : '18420'},
		{'location' :  'Cripple Creek' , 'fips' : '18530'}, {'location' :  'Crook' , 'fips' : '18640'},
		{'location' :  'Crowley' , 'fips' : '18750'}, {'location' :  'Dacono' , 'fips' : '19080'},
		{'location' :  'De Beque' , 'fips' : '19355'}, {'location' :  'Deer Trail' , 'fips' : '19630'},
		{'location' :  'Del Norte' , 'fips' : '19795'}, {'location' :  'Delta' , 'fips' : '19850'},
		{'location' :  'Denver' , 'fips' : '20000'}, {'location' :  'Dillon' , 'fips' : '20440'},
		{'location' :  'Dinosaur' , 'fips' : '20495'}, {'location' :  'Dolores' , 'fips' : '20770'},
		{'location' :  'Dove Creek' , 'fips' : '21265'}, {'location' :  'Durango' , 'fips' : '22035'},
		{'location' :  'Eads' , 'fips' : '22145'}, {'location' :  'Eagle' , 'fips' : '22200'},
		{'location' :  'Eaton' , 'fips' : '22860'}, {'location' :  'Eckley' , 'fips' : '23025'},
		{'location' :  'Edgewater' , 'fips' : '23135'}, {'location' :  'Elizabeth' , 'fips' : '23740'},
		{'location' :  'Empire' , 'fips' : '24620'}, {'location' :  'Englewood' , 'fips' : '24785'},
		{'location' :  'Erie' , 'fips' : '24950'}, {'location' :  'Estes Park' , 'fips' : '25115'},
		{'location' :  'Evans' , 'fips' : '25280'}, {'location' :  'Fairplay' , 'fips' : '25610'},
		{'location' :  'Federal Heights' , 'fips' : '26270'}, {'location' :  'Firestone' , 'fips' : '26600'},
		{'location' :  'Flagler' , 'fips' : '26765'}, {'location' :  'Fleming' , 'fips' : '26875'},
		{'location' :  'Florence' , 'fips' : '27040'}, {'location' :  'Fort Collins' , 'fips' : '27425'},
		{'location' :  'Fort Lupton' , 'fips' : '27700'}, {'location' :  'Fort Morgan' , 'fips' : '27810'},
		{'location' :  'Fountain' , 'fips' : '27865'}, {'location' :  'Fowler' , 'fips' : '27975'},
		{'location' :  'Foxfield' , 'fips' : '28105'}, {'location' :  'Fraser' , 'fips' : '28305'},
		{'location' :  'Frederick' , 'fips' : '28360'}, {'location' :  'Frisco' , 'fips' : '28690'},
		{'location' :  'Fruita' , 'fips' : '28745'}, {'location' :  'Garden City' , 'fips' : '29185'},
		{'location' :  'Genoa' , 'fips' : '29680'}, {'location' :  'Georgetown' , 'fips' : '29735'},
		{'location' :  'Gilcrest' , 'fips' : '29955'}, {'location' :  'Glendale' , 'fips' : '30340'},
		{'location' :  'Glenwood Springs' , 'fips' : '30780'}, {'location' :  'Golden' , 'fips' : '30835'},
		{'location' :  'Granada' , 'fips' : '31550'}, {'location' :  'Granby' , 'fips' : '31605'},
		{'location' :  'Grand Junction' , 'fips' : '31660'}, {'location' :  'Grand Lake' , 'fips' : '31715'},
		{'location' :  'Greeley' , 'fips' : '32155'}, {'location' :  'Green Mountain Falls' , 'fips' : '32650'},
		{'location' :  'Greenwood Village' , 'fips' : '33035'}, {'location' :  'Grover' , 'fips' : '33310'},
		{'location' :  'Gunnison' , 'fips' : '33640'}, {'location' :  'Gypsum' , 'fips' : '33695'},
		{'location' :  'Hartman' , 'fips' : '34520'}, {'location' :  'Haswell' , 'fips' : '34740'},
		{'location' :  'Haxtun' , 'fips' : '34960'}, {'location' :  'Hayden' , 'fips' : '35070'},
		{'location' :  'Hillrose' , 'fips' : '36610'}, {'location' :  'Holly' , 'fips' : '37215'},
		{'location' :  'Holyoke' , 'fips' : '37270'}, {'location' :  'Hooper' , 'fips' : '37380'},
		{'location' :  'Hot Sulphur Springs' , 'fips' : '37600'}, {'location' :  'Hotchkiss' , 'fips' : '37545'},
		{'location' :  'Hudson' , 'fips' : '37820'}, {'location' :  'Hugo' , 'fips' : '37875'},
		{'location' :  'Idaho Springs' , 'fips' : '38370'}, {'location' :  'Ignacio' , 'fips' : '38535'},
		{'location' :  'Iliff' , 'fips' : '38590'}, {'location' :  'Jamestown' , 'fips' : '39195'},
		{'location' :  'Johnstown' , 'fips' : '39855'}, {'location' :  'Julesburg' , 'fips' : '39965'},
		{'location' :  'Keenesburg' , 'fips' : '40185'}, {'location' :  'Kersey' , 'fips' : '40515'}, {'location' :  'Keystone' , 'fips' : '40550'}, 
		{'location' :  'Kim' , 'fips' : '40570'}, {'location' :  'Kiowa' , 'fips' : '40790'},
		{'location' :  'Kit Carson' , 'fips' : '41010'}, {'location' :  'Kremmling' , 'fips' : '41560'},
		{'location' :  'La Jara' , 'fips' : '42055'}, {'location' :  'La Junta' , 'fips' : '42110'},
		{'location' :  'La Salle' , 'fips' : '43605'}, {'location' :  'La Veta' , 'fips' : '44100'},
		{'location' :  'Lafayette' , 'fips' : '41835'}, {'location' :  'Lake City' , 'fips' : '42330'},
		{'location' :  'Lakeside' , 'fips' : '42495'}, {'location' :  'Lakewood' , 'fips' : '43000'},
		{'location' :  'Lamar' , 'fips' : '43110'}, {'location' :  'Larkspur' , 'fips' : '43550'},
		{'location' :  'Las Animas' , 'fips' : '43660'}, {'location' :  'Leadville' , 'fips' : '44320'},
		{'location' :  'Limon' , 'fips' : '44980'}, {'location' :  'Littleton' , 'fips' : '45255'},
		{'location' :  'Lochbuie' , 'fips' : '45530'}, {'location' :  'Log Lane Village' , 'fips' : '45695'},
		{'location' :  'Lone Tree' , 'fips' : '45955'}, {'location' :  'Longmont' , 'fips' : '45970'},
		{'location' :  'Louisville' , 'fips' : '46355'}, {'location' :  'Loveland' , 'fips' : '46465'},
		{'location' :  'Lyons' , 'fips' : '47070'}, {'location' :  'Manassa' , 'fips' : '48060'},
		{'location' :  'Mancos' , 'fips' : '48115'}, {'location' :  'Manitou Springs' , 'fips' : '48445'},
		{'location' :  'Manzanola' , 'fips' : '48500'}, {'location' :  'Marble' , 'fips' : '48555'},
		{'location' :  'Mead' , 'fips' : '49600'}, {'location' :  'Meeker' , 'fips' : '49875'},
		{'location' :  'Merino' , 'fips' : '50040'}, {'location' :  'Milliken' , 'fips' : '50480'},
		{'location' :  'Minturn' , 'fips' : '50920'}, {'location' :  'Moffat' , 'fips' : '51250'},
		{'location' :  'Monte Vista' , 'fips' : '51635'}, {'location' :  'Montezuma' , 'fips' : '51690'},
		{'location' :  'Montrose' , 'fips' : '51745'}, {'location' :  'Monument' , 'fips' : '51800'},
		{'location' :  'Morrison' , 'fips' : '52075'}, {'location' :  'Mount Crested Butte' , 'fips' : '52570'},
		{'location' :  'Mountain View' , 'fips' : '52350'}, {'location' :  'Mountain Village' , 'fips' : '52550'},
		{'location' :  'Naturita' , 'fips' : '53120'}, {'location' :  'Nederland' , 'fips' : '53175'},
		{'location' :  'New Castle' , 'fips' : '53395'}, {'location' :  'Northglenn' , 'fips' : '54330'},
		{'location' :  'Norwood' , 'fips' : '54880'}, {'location' :  'Nucla' , 'fips' : '54935'},
		{'location' :  'Nunn' , 'fips' : '55045'}, {'location' :  'Oak Creek' , 'fips' : '55155'},
		{'location' :  'Olathe' , 'fips' : '55540'}, {'location' :  'Olney Springs' , 'fips' : '55705'},
		{'location' :  'Ophir' , 'fips' : '55870'}, {'location' :  'Orchard City' , 'fips' : '55980'},
		{'location' :  'Ordway' , 'fips' : '56145'}, {'location' :  'Otis' , 'fips' : '56365'},
		{'location' :  'Ouray' , 'fips' : '56420'}, {'location' :  'Ovid' , 'fips' : '56475'},
		{'location' :  'Pagosa Springs' , 'fips' : '56860'}, {'location' :  'Palisade' , 'fips' : '56970'},
		{'location' :  'Palmer Lake' , 'fips' : '57025'}, {'location' :  'Paoli' , 'fips' : '57245'},
		{'location' :  'Paonia' , 'fips' : '57300'}, {'location' :  'Parachute' , 'fips' : '57400'},
		{'location' :  'Parker' , 'fips' : '57630'}, {'location' :  'Peetz' , 'fips' : '58235'},
		{'location' :  'Pierce' , 'fips' : '59005'}, {'location' :  'Pitkin' , 'fips' : '59830'},
		{'location' :  'Platteville' , 'fips' : '60160'}, {'location' :  'Poncha Springs' , 'fips' : '60600'},
		{'location' :  'Pritchett' , 'fips' : '61315'}, {'location' :  'Pueblo' , 'fips' : '62000'},
		{'location' :  'Ramah' , 'fips' : '62660'}, {'location' :  'Rangely' , 'fips' : '62880'},
		{'location' :  'Raymer (New Raymer)' , 'fips' : '63045'}, {'location' :  'Red Cliff' , 'fips' : '63265'},
		{'location' :  'Rico' , 'fips' : '64090'}, {'location' :  'Ridgway' , 'fips' : '64200'},
		{'location' :  'Rifle' , 'fips' : '64255'}, {'location' :  'Rockvale' , 'fips' : '64970'},
		{'location' :  'Rocky Ford' , 'fips' : '65190'}, {'location' :  'Romeo' , 'fips' : '65740'},
		{'location' :  'Rye' , 'fips' : '66895'}, {'location' :  'Saguache' , 'fips' : '67005'},
		{'location' :  'Salida' , 'fips' : '67280'}, {'location' :  'San Luis' , 'fips' : '68105'},
		{'location' :  'Sanford' , 'fips' : '67830'}, {'location' :  'Sawpit' , 'fips' : '68655'},
		{'location' :  'Sedgwick' , 'fips' : '68930'}, {'location' :  'Seibert' , 'fips' : '69040'},
		{'location' :  'Severance' , 'fips' : '69150'}, {'location' :  'Sheridan' , 'fips' : '69645'},
		{'location' :  'Sheridan Lake' , 'fips' : '69700'}, {'location' :  'Silt' , 'fips' : '70195'},
		{'location' :  'Silver Cliff' , 'fips' : '70250'}, {'location' :  'Silver Plume' , 'fips' : '70360'},
		{'location' :  'Silverthorne' , 'fips' : '70525'}, {'location' :  'Silverton' , 'fips' : '70580'},
		{'location' :  'Simla' , 'fips' : '70635'}, {'location' :  'Snowmass Village' , 'fips' : '71755'},
		{'location' :  'South Fork' , 'fips' : '72395'}, {'location' :  'Springfield' , 'fips' : '73330'},
		{'location' :  'Starkville' , 'fips' : '73715'}, {'location' :  'Steamboat Springs' , 'fips' : '73825'},
		{'location' :  'Sterling' , 'fips' : '73935'}, {'location' :  'Stratton' , 'fips' : '74485'},
		{'location' :  'Sugar City' , 'fips' : '74815'}, {'location' :  'Superior' , 'fips' : '75640'},
		{'location' :  'Swink' , 'fips' : '75970'}, {'location' :  'Telluride' , 'fips' : '76795'},
		{'location' :  'Thornton' , 'fips' : '77290'}, {'location' :  'Timnath' , 'fips' : '77510'},
		{'location' :  'Trinidad' , 'fips' : '78610'}, {'location' :  'Two Buttes' , 'fips' : '79270'},
		{'location' :  'Vail' , 'fips' : '80040'}, {'location' :  'Victor' , 'fips' : '80865'},
		{'location' :  'Vilas' , 'fips' : '81030'}, {'location' :  'Vona' , 'fips' : '81690'},
		{'location' :  'Walden' , 'fips' : '82130'}, {'location' :  'Walsenburg' , 'fips' : '82350'},
		{'location' :  'Walsh' , 'fips' : '82460'}, {'location' :  'Ward' , 'fips' : '82735'},
		{'location' :  'Wellington' , 'fips' : '83230'}, {'location' :  'Westcliffe' , 'fips' : '83450'},
		{'location' :  'Westminster' , 'fips' : '83835'}, {'location' :  'Wheat Ridge' , 'fips' : '84440'},
		{'location' :  'Wiggins' , 'fips' : '84770'}, {'location' :  'Wiley' , 'fips' : '85045'},
		{'location' :  'Williamsburg' , 'fips' : '85155'}, {'location' :  'Windsor' , 'fips' : '85485'},
		{'location' :  'Winter Park' , 'fips' : '85705'}, {'location' :  'Woodland Park' , 'fips' : '86090'},
		{'location' :  'Wray' , 'fips' : '86310'}, {'location' :  'Yampa' , 'fips' : '86475'}]

//places
var place =[{'location' :  'Acres Green CDP' , 'fips' : '00320'}, {'location' :  'Aetna Estates CDP' , 'fips' : '00620'},
		{'location' :  'Air Force Academy CDP' , 'fips' : '00870'}, {'location' :  'Alamosa East CDP' , 'fips' : '01145'},
		{'location' :  'Allenspark CDP' , 'fips' : '01420'}, {'location' :  'Alpine CDP' , 'fips' : '01640'},
		{'location' :  'Altona CDP' , 'fips' : '01740'}, {'location' :  'Amherst CDP' , 'fips' : '01915'},
		{'location' :  'Applewood CDP' , 'fips' : '02575'}, {'location' :  'Arboles CDP' , 'fips' : '02905'},
		{'location' :  'Aristocrat Ranchettes CDP' , 'fips' : '03015'}, {'location' :  'Aspen Park CDP' , 'fips' : '03730'},
		{'location' :  'Atwood CDP' , 'fips' : '03840'}, {'location' :  'Avondale CDP' , 'fips' : '04165'},
		{'location' :  'Bark Ranch CDP' , 'fips' : '04620'}, {'location' :  'Battlement Mesa CDP' , 'fips' : '05120'},
		{'location' :  'Berkley CDP' , 'fips' : '06172'}, {'location' :  'Beulah Valley CDP' , 'fips' : '06602'},
		{'location' :  'Black Forest CDP' , 'fips' : '06970'}, {'location' :  'Blende CDP' , 'fips' : '07245'},
		{'location' :  'Blue Sky CDP' , 'fips' : '07420'}, {'location' :  'Bonanza Mountain Estates CDP' , 'fips' : '07580'},
		{'location' :  'Brandon CDP' , 'fips' : '08290'}, {'location' :  'Brick Center CDP' , 'fips' : '08530'},
		{'location' :  'Byers CDP' , 'fips' : '10985'}, {'location' :  'Capulin CDP' , 'fips' : '11975'},
		{'location' :  'Cascade-Chipita Park CDP' , 'fips' : '12325'}, {'location' :  'Castle Pines CDP' , 'fips' : '12387'},
		{'location' :  'Cathedral CDP' , 'fips' : '12450'}, {'location' :  'Catherine CDP' , 'fips' : '12460'},
		{'location' :  'Cattle Creek CDP' , 'fips' : '12470'}, {'location' :  'Chacra CDP' , 'fips' : '12945'},
		{'location' :  'Cherry Creek CDP' , 'fips' : '13590'}, {'location' :  'Cimarron Hills CDP' , 'fips' : '14587'},
		{'location' :  'Clifton CDP' , 'fips' : '15165'}, {'location' :  'Coal Creek CDP' , 'fips' : '15302'},
		{'location' :  'Coaldale CDP' , 'fips' : '15440'}, {'location' :  'Colona CDP' , 'fips' : '15825'},
		{'location' :  'Colorado City CDP' , 'fips' : '15935'}, {'location' :  'Columbine CDP' , 'fips' : '16110'},
		{'location' :  'Comanche Creek CDP' , 'fips' : '16465'}, {'location' :  'Conejos CDP' , 'fips' : '16715'},
		{'location' :  'Copper Mountain CDP' , 'fips' : '17150'}, {'location' :  'Cotopaxi CDP' , 'fips' : '17485'},
		{'location' :  'Crisman CDP' , 'fips' : '18585'}, {'location' :  'Dakota Ridge CDP' , 'fips' : '19150'},
		{'location' :  'Derby CDP' , 'fips' : '20275'}, {'location' :  'Divide CDP' , 'fips' : '20605'},
		{'location' :  'Dotsero CDP' , 'fips' : '21155'}, {'location' :  'Dove Valley CDP' , 'fips' : '21330'},
		{'location' :  'Downieville-Lawson-Dumont CDP' , 'fips' : '21390'}, {'location' :  'East Pleasant View CDP' , 'fips' : '22575'},
		{'location' :  'Edwards CDP' , 'fips' : '23300'}, {'location' :  'El Jebel CDP' , 'fips' : '23795'},
		{'location' :  'El Moro CDP' , 'fips' : '24290'}, {'location' :  'Elbert CDP' , 'fips' : '23520'},
		{'location' :  'Eldora CDP' , 'fips' : '23575'}, {'location' :  'Eldorado Springs CDP' , 'fips' : '23630'},
		{'location' :  'Ellicott CDP' , 'fips' : '24235'}, {'location' :  'Evergreen CDP' , 'fips' : '25390'},
		{'location' :  'Fairmount CDP' , 'fips' : '25550'}, {'location' :  'Florissant CDP' , 'fips' : '27095'},
		{'location' :  'Floyd Hill CDP' , 'fips' : '27175'}, {'location' :  'Fort Carson CDP' , 'fips' : '27370'},
		{'location' :  'Fort Garland CDP' , 'fips' : '27535'}, {'location' :  'Franktown CDP' , 'fips' : '28250'},
		{'location' :  'Fruitvale CDP' , 'fips' : '28800'}, {'location' :  'Fulford CDP' , 'fips' : '28830'},
		{'location' :  'Garfield CDP' , 'fips' : '29295'}, {'location' :  'Genesee CDP' , 'fips' : '29625'},
		{'location' :  'Gerrard CDP' , 'fips' : '29845'}, {'location' :  'Glendale CDP' , 'fips' : '30350'},
		{'location' :  'Gleneagle CDP' , 'fips' : '30420'}, {'location' :  'Gold Hill CDP' , 'fips' : '30945'},
		{'location' :  'Goldfield CDP' , 'fips' : '30890'}, {'location' :  'Grand View Estates CDP' , 'fips' : '31935'},
		{'location' :  'Guffey CDP' , 'fips' : '33420'}, {'location' :  'Gunbarrel CDP' , 'fips' : '33502'},
		{'location' :  'Hasty CDP' , 'fips' : '34685'}, {'location' :  'Heeney CDP' , 'fips' : '35400'},
		{'location' :  'Hidden Lake CDP' , 'fips' : '35860'}, {'location' :  'Highlands Ranch CDP' , 'fips' : '36410'},
		{'location' :  'Hoehne CDP' , 'fips' : '36940'}, {'location' :  'Holly Hills CDP' , 'fips' : '37220'},
		{'location' :  'Howard CDP' , 'fips' : '37655'}, {'location' :  'Idalia CDP' , 'fips' : '38425'},
		{'location' :  'Idledale CDP' , 'fips' : '38480'}, {'location' :  'Indian Hills CDP' , 'fips' : '38810'},
		{'location' :  'Inverness CDP' , 'fips' : '38910'}, {'location' :  'Jackson Lake CDP' , 'fips' : '39160'},
		{'location' :  'Jansen CDP' , 'fips' : '39250'}, {'location' :  'Joes CDP' , 'fips' : '39745'},
		{'location' :  'Johnson Village CDP' , 'fips' : '39800'}, {'location' :  'Ken Caryl CDP' , 'fips' : '40377'},
		{'location' :  'Kirk CDP' , 'fips' : '40900'},
		{'location' :  'Kittredge CDP' , 'fips' : '41065'}, {'location' :  'La Junta Gardens CDP' , 'fips' : '42165'},
		{'location' :  'Laird CDP' , 'fips' : '42000'}, {'location' :  'Laporte CDP' , 'fips' : '43220'},
		{'location' :  'Lazy Acres CDP' , 'fips' : '44270'}, {'location' :  'Leadville North CDP' , 'fips' : '44375'},
		{'location' :  'Lewis CDP' , 'fips' : '44595'}, {'location' :  'Leyner CDP' , 'fips' : '44695'},
		{'location' :  'Lincoln Park CDP' , 'fips' : '45145'}, {'location' :  'Loghill Village CDP' , 'fips' : '45680'},
		{'location' :  'Loma CDP' , 'fips' : '45750'}, {'location' :  'Louviers CDP' , 'fips' : '46410'},
		{'location' :  'Lynn CDP' , 'fips' : '47015'}, {'location' :  'Maybell CDP' , 'fips' : '49325'},
		{'location' :  'Maysville CDP' , 'fips' : '49490'}, {'location' :  'McCoy CDP' , 'fips' : '47345'},
		{'location' :  'Meridian CDP' , 'fips' : '50012'}, {'location' :  'Midland CDP' , 'fips' : '50380'},
		{'location' :  'Morgan Heights CDP' , 'fips' : '51975'}, {'location' :  'Mountain Meadows CDP' , 'fips' : '52210'},
		{'location' :  'Mulford CDP' , 'fips' : '52820'}, {'location' :  'Niwot CDP' , 'fips' : '53780'},
		{'location' :  'No Name CDP' , 'fips' : '53875'}, {'location' :  'Norrie CDP' , 'fips' : '53945'},
		{'location' :  'North La Junta CDP' , 'fips' : '54495'}, {'location' :  'North Washington CDP' , 'fips' : '54750'},
		{'location' :  'Orchard CDP' , 'fips' : '55925'}, {'location' :  'Orchard Mesa CDP' , 'fips' : '56035'},
		{'location' :  'Padroni CDP' , 'fips' : '56695'}, {'location' :  'Paragon Estates CDP' , 'fips' : '57445'},
		{'location' :  'Parshall CDP' , 'fips' : '57850'}, {'location' :  'Penrose CDP' , 'fips' : '58400'},
		{'location' :  'Peoria CDP' , 'fips' : '58510'}, {'location' :  'Perry Park CDP' , 'fips' : '58592'},
		{'location' :  'Peyton CDP' , 'fips' : '58675'}, {'location' :  'Phippsburg CDP' , 'fips' : '58758'},
		{'location' :  'Piedra CDP' , 'fips' : '58960'}, {'location' :  'Pine Brook Hill CDP' , 'fips' : '59240'},
		{'location' :  'Ponderosa Park CDP' , 'fips' : '60655'}, {'location' :  'Portland CDP' , 'fips' : '60765'},
		{'location' :  'Pueblo West CDP' , 'fips' : '62220'}, {'location' :  'Red Feather Lakes CDP' , 'fips' : '63320'},
		{'location' :  'Redlands CDP' , 'fips' : '63375'}, {'location' :  'Redstone CDP' , 'fips' : '63650'},
		{'location' :  'Redvale CDP' , 'fips' : '63705'}, {'location' :  'Rock Creek Park CDP' , 'fips' : '64870'},
		{'location' :  'Rollinsville CDP' , 'fips' : '65685'}, {'location' :  'Roxborough Park CDP' , 'fips' : '66197'},
		{'location' :  'Saddle Ridge CDP' , 'fips' : '66995'}, {'location' :  'Salt Creek CDP' , 'fips' : '67445'},
		{'location' :  'San Acacio CDP' , 'fips' : '67500'}, {'location' :  'Security-Widefield CDP' , 'fips' : '68847'},
		{'location' :  'Sedalia CDP' , 'fips' : '68875'}, {'location' :  'Segundo CDP' , 'fips' : '68985'},
		{'location' :  'Seven Hills CDP' , 'fips' : '69110'}, {'location' :  'Shaw Heights CDP' , 'fips' : '69480'},
		{'location' :  'Sherrelwood CDP' , 'fips' : '69810'}, {'location' :  'Smeltertown CDP' , 'fips' : '71625'},
		{'location' :  'Snyder CDP' , 'fips' : '71790'}, {'location' :  'Southern Ute CDP' , 'fips' : '72320'},
		{'location' :  'St. Ann Highlands CDP' , 'fips' : '67040'}, {'location' :  'St. Mary\'s CDP' , 'fips' : '67142'},
		{'location' :  'Stonegate CDP' , 'fips' : '74080'}, {'location' :  'Stonewall Gap CDP' , 'fips' : '74275'},
		{'location' :  'Strasburg CDP' , 'fips' : '74375'}, {'location' :  'Stratmoor CDP' , 'fips' : '74430'},
		{'location' :  'Sugarloaf CDP' , 'fips' : '74980'}, {'location' :  'Sunshine CDP' , 'fips' : '75585'},
		{'location' :  'Tabernash CDP' , 'fips' : '76190'}, {'location' :  'Tall Timber CDP' , 'fips' : '76325'},
		{'location' :  'The Pinery CDP' , 'fips' : '77235'}, {'location' :  'Todd Creek CDP' , 'fips' : '77757'},
		{'location' :  'Towaoc CDP' , 'fips' : '78280'}, {'location' :  'Towner CDP' , 'fips' : '78335'},
		{'location' :  'Trail Side CDP' , 'fips' : '78345'}, {'location' :  'Twin Lakes CDP' , 'fips' : '79100'},
		{'location' :  'Twin Lakes CDP' , 'fips' : '79105'}, {'location' :  'Upper Bear Creek CDP' , 'fips' : '79785'},
		{'location' :  'Valdez CDP' , 'fips' : '80095'}, {'location' :  'Valmont CDP' , 'fips' : '80370'},
		{'location' :  'Vernon CDP' , 'fips' : '80755'}, {'location' :  'Vineland CDP' , 'fips' : '81305'},
		{'location' :  'Watkins CDP' , 'fips' : '82905'}, {'location' :  'Welby CDP' , 'fips' : '83120'},
		{'location' :  'Weldona CDP' , 'fips' : '83175'}, {'location' :  'West Pleasant View CDP' , 'fips' : '84042'},
		{'location' :  'Westcreek CDP' , 'fips' : '83500'}, {'location' :  'Weston CDP' , 'fips' : '84000'},
		{'location' :  'Wolcott CDP' , 'fips' : '85760'}, {'location' :  'Woodmoor CDP' , 'fips' : '86117'}]

//Profile selection -- Removing Comparisons and CDPs  
var profile = [{'location' :  'Select Profile' , 'fips' : ''},{'location' :  'Region' , 'fips' : 'region'}, {'location' :  'County' , 'fips' : 'county'},
		{'location' :  'Municipality', 'fips' : 'municipality'}
	/*
        {'location' :  'Census Designated Place' , 'fips' : 'place'},
		{'location' :  'Regional Comparison' , 'fips' : 'regioncomp'}, {'location' :  'County Comparison' , 'fips' : 'countycomp'},
        {'location' :  'Municipal Comparison', 'fips' : 'municipalitycomp'}
	,   {'location' :  'Census Designated Place Comparison' , 'fips' : 'placecomp'}
	*/
	]	

if(level == 'region') { 
	var locarr = region;
    if(callpg == 'lookup'){
		var staterec = {'optgroup' : 'State Total','location' : 'Colorado', 'regnum' : '000'};
		locarr.unshift(staterec);
	}
  }

if(callpg == 'profile') { //removes "Colorado" from the county array when called by the profile
	if(level == 'county') {
		var tmpcty = county;
		tmpcty.shift();
		var locarr = tmpcty;
	}
} else {
   if(level == 'county') {var locarr = county};
}
if(level == 'municipality') { var locarr = municipality};
if(level == 'place') { var locarr = place};
if(level == 'regioncomp') {var locarr = region};
if(level == 'countycomp') {var locarr = county};
if(level == 'municipalitycomp') { var locarr = municipality};
if(level == 'placecomp') { var locarr = place};
if(level == 'profile') { var locarr = profile};

var sel = document.getElementById(ddid);
sel.innerHTML = "";

if(level == 'region' || level == 'regioncomp') {
	var groups = [... new Set(locarr.map(tag => tag.optgroup))];

	for(i = 0; i < groups.length; i++){
		var groupfilt = locarr.filter(d => (d.optgroup == groups[i]));
	    var grp = document.createElement("optgroup");
		grp.id = "regiongrp" + i;
		grp.label = groups[i];
		for(j = 0; j < groupfilt.length; j++) {
			var optTxt = document.createElement("option");
			optTxt.textContent  = groupfilt[j].location;
			optTxt.value = groupfilt[j].regnum;
			grp.appendChild(optTxt);
		}
		sel.add(grp)
	}
	
    } else {
	if(callpg == 'jobs'){
		var sel = document.getElementById(ddid);
		for(var i = 0; i < locarr.length; i++) {
			var el = document.createElement("option");
			el.textContent = locarr[i].location;
			if(["001","005","013","014","031","035","059"].includes(locarr[i].fips)){
				el.style.color = "#A51C30"
			}
			el.value = locarr[i].fips;
			sel.appendChild(el);
		}
	} else {
	if(callpg == 'lookup') {
		locarr.shift()
	}
	 var sel = document.getElementById(ddid);
		for(var i = 0; i < locarr.length; i++) {
			var el = document.createElement("option");
			el.textContent = locarr[i].location;
			el.value = locarr[i].fips;
			sel.appendChild(el);
		}
	}
	}
}; 
// popDropdown	


function popCtyDrop(regnum,ddid){
//popCtyDrop  Populates the region dropdown and facilitates drill down in Demogaphic Dashboard and other applicaitons
	reg_num = parseInt(regnum);
	reg_name = regionName(reg_num);
	fips_list = regionCOL(reg_num);
	var outlist = [];
	outlist.push({'fips' : regnum, 'location' : reg_name});

	for(i = 0; i < fips_list[0].fips.length; i++){
		var cnum = parseInt(fips_list[0].fips[i]);
		outlist.push({'fips' : fips_list[0].fips[i], 'location' : countyName(cnum)});
	}

var sel = document.getElementById(ddid);
    sel.innerHTML = "";
	for(var i = 0; i < outlist.length; i++) {
		var el = document.createElement("option");
		el.textContent = outlist[i].location;
		el.value = outlist[i].fips;
		sel.appendChild(el);
	}
} 
// popCtyDrop



function regionCOL(regnum) {
//regionCOL  returns te fips codes and color codes for a input region number
//Color Pallettes
//Geographic Regions  DOLA Reds
//Management Regions DOLA Purples
//Metro Regions DOLA tree greens
//Micro Regions DOLA Left Mountain Greens
//Denver DOLA Rignh Mountain Teal
	var fips = []; 
if(regnum == 0) {fips.push({'fips' : ['001', '003', '005', '007', '009', '011', '013', '014', '015', '017', '019', '021', '023', '025', '027', 
									'029', '031', '033', '035', '037', '039', '041', '043', '045', '047', '049', '051', '053', '055', '057', 
									'059', '061', '063', '065', '067', '069', '071', '073', '075', '077', '079', '081', '083', '085', '087', 
									'089', '091', '093', '095', '097', '099', '101', '103', '105', '107', '109', '111', '113', '115', '117', 
									'119', '121', '123', '125'], 'color' : ''})};
if(regnum == 1) {fips.push({'fips' : ['015', '019', '027', '043', '047', '055', '065', '071', '093'], 'color' : '#EE6677'})};
if(regnum == 2) {fips.push({'fips' : ['009', '011', '017', '025', '039', '061', '063', '073', '075', '087', '089', '095', '099', '115', '121', '125'],  'color' : '#228833'})};
if(regnum == 3) {fips.push({'fips' : ['001', '005', '013', '014', '031', '035', '041', '059', '069', '101','119','123'],  'color' : '#4477AA'})};
if(regnum == 4) {fips.push({'fips' : ['003', '021', '023', '079', '105', '109'],  'color' : '#CCBB44'})};
if(regnum == 5) {fips.push({'fips' : ['007', '029', '033', '037', '045', '049', '051', '053', '057', '067', '077', '081', '083', '085', '091', '097', '103', '107', '111', '113', '117'],  'color' : '#66CCEE'})};
if(regnum == 6) {fips.push({'fips' : ['075','087','095','115','121','125'], 'color' : '#AA3377'})};
if(regnum == 7) {fips.push({'fips' : ['069','123'],  'color' : '#BBBBBB'})};
if(regnum == 8) {fips.push({'fips' : ['001','005','013','014','019','031','035','047','059'], 'color' : '#505050'})};
if(regnum == 9) {fips.push({'fips' : ['041','093','119'],  'color' : '#44AA99'})};
if(regnum == 10) {fips.push({'fips' : ['017','039','063','073'], 'color' : '#117733'})};
if(regnum == 11) {fips.push({'fips' : ['009','011','025','061','089','099'],  'color' : '#332288'})};
if(regnum == 12) {fips.push({'fips' : ['101'],  'color' : '#DDCC77'})};
if(regnum == 13) {fips.push({'fips' : ['003','021','023','079','105','109'],  'color' : '#999933'})};
if(regnum == 14) {fips.push({'fips' : ['007','033','067','083','111'],  'color' : '#CC6677'})};
if(regnum == 15) {fips.push({'fips' : ['029','051','053','085','091','113'], 'color' : '#F8F8F8'})};
if(regnum == 16) {fips.push({'fips' : ['045','077','081','103'],  'color' : '#AA4499'})};
if(regnum == 17) {fips.push({'fips' : ['037','049','057','097','107','117'], 'color' : '#DDDDDD'})};
if(regnum == 18) {fips.push({'fips' : ['015','027','043','065'], 'color' : '#BBCC33'})};
if(regnum == 19) {fips.push({'fips' : ['055','071'],  'color' : '#AAAA00'})};
if(regnum == 20) {fips.push({'fips' : ['013'], 'color' : '#77AADD'})};
if(regnum == 21) {fips.push({'fips' : ['041','119'], 'color' : '#EE8866'})};
if(regnum == 22) {fips.push({'fips' : ['001','005','014','019','031','035','039','047','059','093'],  'color' : '#EEDD88'})};
if(regnum == 23) {fips.push({'fips' : ['069'],  'color' : '#FFAABB'})};
if(regnum == 24) {fips.push({'fips' : ['077'],  'color' : '#99DDFF'})};
if(regnum == 25) {fips.push({'fips' : ['123'],  'color' : '#44BB99'})};
if(regnum == 26) {fips.push({'fips' : ['101'],  'color' : '#DDCC77'})};
if(regnum == 27) {fips.push({'fips' : ['117'], 'color' : '#E69F00'})};
if(regnum == 28) {fips.push({'fips' : ['043'],  'color' : '#56B4E9'})};
if(regnum == 29) {fips.push({'fips' : ['081'],  'color' : '#009E73'})};
if(regnum == 30) {fips.push({'fips' : ['067'],  'color' : '#F0E442'})};
if(regnum == 31) {fips.push({'fips' : ['037'],  'color' : '#0072B2'})};
if(regnum == 32) {fips.push({'fips' : ['087'],  'color' : '#D55E00'})};
if(regnum == 33) {fips.push({'fips' : ['045','097'],  'color' : '#CC79A7'})};
if(regnum == 34) {fips.push({'fips' : ['085','091'],  'color' : '#696969'})};
if(regnum == 35) {fips.push({'fips' : ['107'], 'color' : '#808080'})};
if(regnum == 36) {fips.push({'fips' : ['075'],  'color' : '#A9A9A9'})};
if(regnum == 37) {fips.push({'fips' : ['001', '005', '014', '031', '035', '059'],  'color' : '#505050'})};
if(regnum == 38) {fips.push({'fips' : ['001', '005', '013', '014', '031', '035', '059'], 'color' : '#C0C0C0'})};
if(regnum == 39) {fips.push({'fips' : ['001', '005', '013', '014', '031', '035', '059', '123'],  'color' : '#D3D3D3'})};

	 return fips;
	};
// regionCOL

function restructureRace(inData) {
//restructureRace restructures SDO race data 
   var output = [];
   var WH, HP, BL, AS, NH, AM, MULTI;
   var ages = [... new Set(inData.map(tag => tag.age))];

    for(i = 0; i < ages.length; i++) {
		var tmp = inData.filter(function(d) {return d.age == ages[i];});
		for(j = 0; j < tmp.length; j++) {
				  if( tmp[j].race_eth == "White alone NH") { WH = tmp[j].population};
				  if( tmp[j].race_eth == "Hispanic") { HP = tmp[j].population};
				  if( tmp[j].race_eth == "Black or African American alone NH") { BL = tmp[j].population};
				  if( tmp[j].race_eth == "Asian alone NH") {AS = tmp[j].population};
				  if( tmp[j].race_eth == "Native Hawaiian or Other Pacific Islander alone NH") {NH = tmp[j].population};
				  if( tmp[j].race_eth == "American Indian and Alaska Native alone NH") {AM = tmp[j].population};
		          if( tmp[j].race_eth == "Two or more NH") {MULTI = tmp[j].population};
				}
		output.push({"fips" : tmp[0].fips, "name" : tmp[0].name, "age" : tmp[0].age, 
					"Hispanic" : HP, 
					"White Alone NH" : WH, 
					"Black Alone NH" : BL, 
					"Asian Alone NH" : AS, 
					"Native Hawaiian/Pacific Islander Alone NH": NH,
					"American Indian, NH" : AM, 
					"Two or More Races, NH": MULTI});
		};

    return output;
};
// restructureRace



function genFilename(outname, type, ext, yr) {
//genFilename generates standardized file names for data and chart download functionss
	 switch(type){
		case 'test' :
		var fileName = "Test values " +  outname + "." + ext;
		break;
		case 'map' :
		    var fileName = outname[0] + " Area Map." + ext;
		break;
		case 'summary' :
		    var fileName = outname + " Basic Statistics Table." + ext;
		break;
		case 'estimate' :
			var fileName = outname + " Population Estimates." + ext;
		break;
		case 'forecast' :
			var fileName = outname + " Population Forecast." + ext;
		break;
		case 'coc' :
			var fileName = outname + " Components of Change." + ext;
		break;
		case 'netmig' :
			var fileName = outname + " Net Migration by Age." + ext;
		break;
		case 'linecoc' :
			var fileName = outname + " Long Term Components of Change." + ext;
		break;
		case 'barcoc' :
			var fileName = outname + " Long Term Components of Change Barchart." + ext;
		break;
		case 'age' :
			var fileName = outname + " Age Categories." + ext;
		break;
		case 'ageest' :
			var fileName = outname + " Age Estimate." + ext;
		break;
		case 'agefor' :
			var fileName = outname + " Age Forecast." + ext;
		break;
		case 'agepyr' :
			var fileName = outname + " Age Pyramid." + ext;
		break;
		case 'popchng' :
			var fileName = outname + " Population Change by Age Group." + ext;
		break;
		case 'line' :
			var fileName = outname + " Single Year of Age by Race " + yr + "." + ext;
		break;
		case 'white' :
			var fileName = outname +  "Single Year of Age by Race White NH " + yr + "." + ext;
		break;
		case 'hisp' :
			var fileName = outname + " Single Year of Age by Race Hispanic " + yr + "." + ext;
		break;
		case 'black' :
			var fileName = outname + " Single Year of Age by Race Black NH " + yr +  "." + ext;
		break;
		case 'asian' :
			var fileName = outname + " Single Year of Age by Race Asian NH " + yr + "." + ext;
		break;
		case 'nhpi' :
			var fileName = outname + " Single Year of Age by Race Native Hawaiian Pacific Islander NH " + yr + "." + ext;
		break;
		case  'amind' :
			var fileName = outname + " Single Year of Age by Race American Indian NH " + yr + "." + ext;
		break;
		case 'multi' :
			var fileName = outname + " Single Year of Age by Race Two or More Races NH " + yr + "." + ext;
		break;
		case 'netmign' :
		   if(ext == "csv"){
			var fileName =  outname + " Net Migration by Age Data." + ext;
		   } else {
			var fileName =  outname + " Net Migration by Age Counts." + ext;
		   }
		break;
		case 'netmigrrate' :
		   if(ext == "csv"){
			var fileName =  outname + " Net Migration by Age Data." + ext;
		   } else {
			var fileName =  outname + " Net Migration by Age Rates." + ext;
		   }
		break;
		case 'netmigwa' :
			var fileName =  outname + " Net Migration by Year." + ext
		break;
		case 'netmigrwa' :
			var fileName =  outname + " Net Migration by Year." + ext
		break;
		case 'nethist' :
			var fileName = outname + " Long Term Trend Births Deaths Migration." + ext
		break;
		case 'hhchart' :
			var fileName = outname + " Household Projections Age Group x Household Type." + ext
		break;
		case 'agechart' :
			var fileName = outname + "Household Projections Household Type and Age Group." + ext
		break;
		case 'housing' :
			var fileName = outname + " Households." + ext
		break;
		case 'income' :
			var fileName = outname + "ACS Income Estimates." + ext
		break;	
		case 'incomesrc' :
			var fileName = outname + " ACS Income Sources." + ext
		break;	
		case 'educatt' :
			var fileName = outname + " ACS Educational Attainment." + ext
		break;
		case 'raceeth' :
			var fileName = outname + " ACS Race and Ethnicity." + ext
		break;
		case 'hhforecast' :
			var fileName = outname + " Household Forecast by Age." + ext
		break;
		case 'popgrowth' :
			var fileName = outname + " Population Growth Table." + ext
		break;
		case 'houseocc' :
			var fileName = outname + " ACS Housing Occupancy and Vacancy Table." + ext
		break;
		case 'housetype' :
			var fileName = outname + " ACS Housing Type Table." + ext
		break;
		case 'houseecon' :
			var fileName = outname + " ACS Housing Cost and Affordability Table." + ext
		break;
		case 'netflow' :
			var fileName = outname + " ACS Net Migration Flows." + ext
		break;
		case 'inflow' :
			var fileName = outname + " ACS In Migration Flows." + ext
		break;
		case 'outflow' :
			var fileName = outname + " ACS Out Migration Flows." + ext
		break;
		case 'LODESBAR' :
			var fileName = outname + " LODES Commuting Summary." + ext
		break;
		case 'LODESFLOW' :
			var fileName = outname + " LODES Communting Flows." + ext
		break;
		} //switch
		
	
return(fileName)
} 
// genFilename


function exportToCsv(cname, type, rows, yr) {
//exporttoCsv  downloads the a selected data file as a CSV
		if(Array.isArray(cname)){
			var outNames = "";
			cname.forEach(d => {outNames = outNames + " " + d; })
			cname = outNames;
		}
	  
        var csvFile = d3.csvFormat(rows);
        var fileName = genFilename(cname, type, "csv", yr)

		
        var blob = new Blob([csvFile], { type: 'text/csv;charset=utf-8;' });
        if (navigator.msSaveBlob) { // IE 10+
            navigator.msSaveBlob(blob, fileName);
        } else {
            var link = document.createElement("a");
            if (link.download !== undefined) { // feature detection
                // Browsers that support HTML5 download attribute
                var url = URL.createObjectURL(blob);
                link.setAttribute("href", url);
                link.setAttribute("download", fileName);
                link.style.visibility = 'hidden';
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
            }
        }
    };
// exportToCsv

function plotDownload(plotdiv,filename,type){
// Downloads Plotly Image
	  if(type == 'agepyr'){
		Plotly.toImage(plotdiv, { format: 'png', width: 900, height: 400 }).then(function (dataURL) {
			var a = document.createElement('a');
			a.href = dataURL;
			a.download = filename;
			document.body.appendChild(a);
			a.click();
			document.body.removeChild(a);
		});
	  } else {
		 Plotly.toImage(plotdiv, { format: 'png', width: 1000, height: 400 }).then(function (dataURL) {
        var a = document.createElement('a');
        a.href = dataURL;
        a.download = filename;
        document.body.appendChild(a);
         a.click();
        document.body.removeChild(a);
    });
	  }
}
// plotDownload


function exportToPng(cname, type, graphDiv, yr){
//exportToPng  Exports plotly trace and layout to PNG file
           fileName = genFilename(cname,type,"png",yr);

		if(Array.isArray(graphDiv)){
			for(i = 0; i < graphDiv.length; i++){
               plotDownload(graphDiv[i].plot,graphDiv[i].fName,type);
			};  //i
		} else {
		var fn = fileName;
		switch(type) {
		case 'map': {
			var count_node = d3.select("svg").node();
	        count_node.setAttribute("viewBox", "0 0 925 500");
	        saveSvgAsPng(count_node, fn);
		} 
		break;
		case 'agepyr' : {
		Plotly.toImage(graphDiv, { format: 'png', width: 900, height: 600}).then(function (dataURL) {
			var a = document.createElement('a');
			a.href = dataURL;
			a.download = fn;
			document.body.appendChild(a);
			 a.click();
			document.body.removeChild(a);
        });
		} 
		break;
	    case 'popchng' : {
		    Plotly.toImage(graphDiv, { format: 'png', width: 900, height: 600}).then(function (dataURL) {
				var a = document.createElement('a');
				a.href = dataURL;
				a.download = fn;
				document.body.appendChild(a);
				 a.click();
				document.body.removeChild(a);
			});
		} 
		break;
	    case 'netflow':
		case 'inflow' : 
		case 'outflow':
		{
		    Plotly.toImage(graphDiv, { format: 'png', width: 900, height: 900}).then(function (dataURL) {
				var a = document.createElement('a');
				a.href = dataURL;
				a.download = fn;
				document.body.appendChild(a);
				 a.click();
				document.body.removeChild(a);
			});
		} 
		break;
		case 'LODESFLOW' :
		{
		    Plotly.toImage(graphDiv, { format: 'png', width: 900, height: 930}).then(function (dataURL) {
				var a = document.createElement('a');
				a.href = dataURL;
				a.download = fn;
				document.body.appendChild(a);
				 a.click();
				document.body.removeChild(a);
			});
		} 
		break;
	  default : {  
	   Plotly.toImage(graphDiv, { format: 'png', width: 1000, height: 500}).then(function (dataURL) {
        var a = document.createElement('a');
        a.href = dataURL;
        a.download = fn;
        document.body.appendChild(a);
         a.click();
        document.body.removeChild(a);
    });
	  }
	break;
	}
		}
};  
// exportToPng

//cat Home Page Table Functions

function returnRank(indata,fips){
//returnRank returns the ranked value of selected field for home page tables
	var rval = 0;
	for(i = 0; i < indata.length; i++){
		if(fips == "000") {
			if(indata[i]['state'] == 8){
				rval = i + 1;
			};
		} else {
			if(indata[i]['county'] == parseInt(fips)){
				rval = i + 1;
			};
		};
	};
 return(rval);
};  
// returnRank 


function genSYA(fips,yrvalue){
//genSYA creates the single year of age data sets for inclusion in table and outputs table to DOM
	var fmt_pct = d3.format(".2%")
	var fmt_comma = d3.format(",");


//Specify fips_list
var fips_list = parseInt(fips); 
   
//extract year value 
 
  var prevyear = yrvalue - 1;
  var year10 = 2030;
  var yr_list = prevyear + "," + yrvalue + "," + year10;
 
 //Generate url
 if(fips == "000") {
 var urlstr = "https://gis.dola.colorado.gov/lookups/sya_regions?reg_num=" + fips_list + "&year=" + yr_list + "&choice=single"
 } else {
     var urlstr = "https://gis.dola.colorado.gov/lookups/sya?county=" + fips_list + "&year=" + yr_list + "&choice=single&group=3"
 }
 
 var totaldata = [];

d3.json(urlstr).then(function(data){
	
   data.forEach(function(obj) {
    if(obj.age >=  0 && obj.age <= 17) {obj.age_cat = "0 to 17"; }
    if(obj.age >= 18 && obj.age <= 24) {obj.age_cat = "18 to 24";}
    if(obj.age >= 25 && obj.age <= 44) {obj.age_cat = "25 to 44"; }
    if(obj.age >= 45 && obj.age <= 64) {obj.age_cat = "45 to 64"; }
    if(obj.age >= 65) {obj.age_cat = "65 +";}
    totaldata.push({'year' : obj.year, 'age_cat' : obj.age_cat, 'totalpopulation' : parseInt(obj.totalpopulation)});
 });

 
      //  Totals
var total_ann = d3.rollup(totaldata, v => d3.sum(v, d => d.totalpopulation), d => d.year);
var total_age = d3.rollup(totaldata, v => d3.sum(v, d => d.totalpopulation), d => d.year, d => d.age_cat);

//Flatten Arrays for output

var total_ann_tmp = [];
for (let [key, value] of total_ann) {
  total_ann_tmp.push({'year' : key, 'totalpopulation' : value});
    };

var total_ann_flat = total_ann_tmp.sort(function(a, b){ return d3.ascending(a['year'], b['year']); });

var total_age_tmp = [];
for (let [key1, value] of total_age) {
for (let[key2, value2] of value) {
   total_age_tmp.push({'year' : key1, 'age_cat' : key2, 'totalpopulation' : value2});
}
};

var total_age_flat = total_age_tmp.sort(function(a,b) {return d3.ascending(a['age_cat'],b['age_cat']);})
            .sort(function(a, b){ return d3.ascending(a['year'], b['year']); });

// Create table array for output
var tbl_arr = []
tbl_arr.push({'age_cat' : '<b>Total</b>', 'prevval' : "<b>"+ fmt_comma(total_ann_flat[0].totalpopulation) + "</b>", 'curval' : "<b>"+fmt_comma(total_ann_flat[1].totalpopulation) + "</b>", 'forval' : "<b>" + fmt_comma(total_ann_flat[2].totalpopulation) +"</b>"});

var ages = [... new Set(total_age_flat.map(tag => tag.age_cat))];
for(i = 0; i < ages.length; i++) {
	var filt = total_age_flat.filter(function(d) {return d.age_cat == ages[i]});
	tbl_arr.push({'age_cat' : ages[i], 'prevval' : fmt_comma(filt[0].totalpopulation), 'curval' : fmt_comma(filt[1].totalpopulation), 'forval' : fmt_comma(filt[2].totalpopulation)});
  };

//Generate Table
d3.select('#PopTab').html("");
$("#PopTab" ).append( "<h2 class='h2_style'>Population Estimates by Age</h2>" );
$("#PopTab").append( "<a href='./assets/lookups/county_sya_lookup.html' target='_blank'>Single Year of Age Lookup</a>" );
$("#PopTab").append("<p></p>");

var tblcolumns2 = ['Ages','Number, '+ prevyear,'Number, '+ yrvalue,'2030 Forecast'];
// Output table 
//d3.select('#PopTab').html("");
var syatab = d3.select('#PopTab')
               .append('table')
               .style('table-layout', 'fixed');
			   
thead = syatab.append('thead');
tbody = syatab.append('tbody');
//Header

thead.append('tr')
   .selectAll('th')
   .data(tblcolumns2).enter()
   .append('th')
   .text(function(d) {return d;})
   .style("border", "1px black solid")
   .style("width","25%")
   .style("text-align", "center")
   .style("font-weight", "bold");
//Rows   

var rows = tbody
    .selectAll('tr')
    .data(tbl_arr).enter()
    .append('tr')
	.attr("width", "25%")
	.attr("border-spacing","0")
	.style("color","black");

//Columns
rows.append('td')
      .style("text-align", "left")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
	  .html(function(m) { return m.age_cat; });
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
      .html(function(m) { return m.prevval; });
rows.append('td')
       .style("text-align", "right") 
	   .style('font-size','10pt')
	   .attr("border-spacing","0")
       .html(function(m) { return m.curval; });
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
      .html(function(m) { return m.forval; });


}); //d3.json
} 
// genSYA


function genCOC(fips,yrvalue){
//genCOC generates components of change data for home page table
	var fmt_pct = d3.format(".2%")
	var fmt_comma = d3.format(",");
// Extracts and summarizes COC data for output table
//Specify fips_list
var fips_list = parseInt(fips); 
   
//extract year value   
 
  var prevyear = yrvalue - 1;
  var yr_list = prevyear + "," + yrvalue;
  
   //Generate url
 var urlstr = "https://gis.dola.colorado.gov/lookups/components?county=" + fips_list + "&year=" + yr_list
  var COCdata = [];
  d3.json(urlstr).then(function(data){
       data.forEach( function(obj){  
      COCdata.push({'year' : obj.year, 'estimate' : parseInt(obj.estimate), 'Births' : parseInt(obj.births),
                    'Deaths' : parseInt(obj.deaths), 'Net Migration' : parseInt(obj.netmig), 'natincr' : parseInt(obj.change)});
  }); 
  
var COC_T = transpose(COCdata);

var COC_fint = Object.values(COC_T);

//Extracting data for table
var tbl_arr = [];
var outname;
var prevVal;
var curVal;
var pctchg;
var forVal;

for(i = 2; i <= 4; i++){
	outname = COC_fint[i][0][0]['name'];
	prevVal = COC_fint[i][0][0]['value'];
	curVal = COC_fint[i][1][0]['value'];
	pctchg = prevVal  == 0 ?  0 : (curVal - prevVal)/prevVal;
	tbl_arr.push({'coc_name' : outname, 'prevval' : fmt_comma(prevVal), 'curval' : fmt_comma(curVal), 'pct_chg' : fmt_pct(pctchg)});
}
//Generate Table
d3.select('#COCTab').html("");
$("#COCTab").append( "<h2 class='h2_style'>Births, Deaths, and Migration</h2>");
$("#COCTab").append("<a href='./assets/lookups/county_coc_lookup.html' target='_blank'>Components of Change Lookup</a>");
$("#COCTab").append("<p></p>");


var tblcolumns2 = ['Component','Number,  ' + prevyear,'Number,  ' + yrvalue,'Change'];
// Output table 

var syatab = d3.select('#COCTab')
               .append('table')
               .style('table-layout', 'fixed');
			   
thead = syatab.append('thead');
tbody = syatab.append('tbody');
//Header
thead.append('tr')
   .selectAll('th')
   .data(tblcolumns2).enter()
   .append('th')
   .text(function(d) {return d;})
   .style("border", "1px black solid")
   .style("width","25%")
   .style("text-align", "center")
   .style("font-weight", "bold");
//Rows   

var rows = tbody
    .selectAll('tr')
    .data(tbl_arr).enter()
    .append('tr')
	.attr("width", "25%")
	.attr("border-spacing","0")
	.style("color","black");

//Columns
rows.append('td')
      .style("text-align", "left")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
	  .html(function(m) { return m.coc_name; });
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
      .html(function(m) { return m.prevval; });
rows.append('td')
       .style("text-align", "right") 
	   .style('font-size','10pt')
	   .attr("border-spacing","0")
       .html(function(m) { return m.curval; });
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
      .html(function(m) { return m.pct_chg; });
	
  }); //d3.json  
} 
// genCOC


function genRaceEth(fips,yrvalue){
//genRaceEth generates race and ethnicity data for home page table
	var fmt_pct = d3.format(".2%")
	var fmt_comma = d3.format(",");

//Specify fips_list
var fips_list = parseInt(fips); 
   
//extract year value 
 
  var prevyear = yrvalue - 1;
  var year10 = 2030;
  var yr_list = prevyear + "," + yrvalue //This is the list for the estimates call.  Year10 is used in the forecast call
 
var age_list = "0";
for(i = 1; i<= 100; i++){
   age_list = age_list + "," + i;
  };
  
 //Generate url
 if(fips == "000") {
 fips_list = "1,3,5,7,9,11,13,14,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125";

 } 
//estimates urls
urlstr_hispest = "https://gis.dola.colorado.gov/lookups/county_sya_race_estimates_current?age="+ age_list + "&county="+ fips_list +"&year="+ yrvalue +"&race=1,2,3,4,5,6&ethnicity=1&group=opt7";
urlstr_nonhispest = "https://gis.dola.colorado.gov/lookups/county_sya_race_estimates_current?age="+ age_list + "&county="+ fips_list +"&year="+ yrvalue +"&race=1,2,3,4,5,6&ethnicity=2&group=opt7";;

//forecast urls
//urlstr_for = "https://gis.dola.colorado.gov/lookups/sya-race-forecast?age=0,18,65&county=" + fips_list + "&year=" + year10 + "&race=1,2,3,4,5&group=opt7";


//Promise Structure
//var prom = [d3.json(urlstr_hispest),d3.json(urlstr_nonhispest),d3.json(urlstr_for)];
var prom = [d3.json(urlstr_hispest),d3.json(urlstr_nonhispest)];

Promise.all(prom).then(function(data){
	var hisp_est = [];
	var nonhisp_est = []
//push out vars and count to number
data[0].forEach(function(obj) {
hisp_est.push({'year' : obj.year, 'sex' : obj.sex, 'population' : Math.round(+obj.count)});
});
    data[1].forEach(function(obj) {
     nonhisp_est.push({'year' : obj.year, 'sex' : obj.sex, 'race' : obj.race, 'population' : Math.round(+obj.count)});
});
/*
    data[2].forEach(function(obj) {
     raceeth_for.push({'year' : obj.year, 'race_eth' : obj.race, 'population' : parseInt(obj.count)});
});



for(i = 0; i < raceeth_for.length; i++){
	if(raceeth_for[i].race_eth == "White non Hispanic"){ raceeth_for[i].race_eth = "White NH"};
	if(raceeth_for[i].race_eth == "Black non Hispanic"){ raceeth_for[i].race_eth = "Black NH"};
	if(raceeth_for[i].race_eth == "American Indian non Hispanic"){ raceeth_for[i].race_eth = "American Indian NH"};
     if(raceeth_for[i].race_eth == "Asian non Hispanic"){ raceeth_for[i].race_eth = "Asian/ Pacific Islander NH"};
};
*/   
//Rolling up the hispanic and non-hispanic datasets
var hisp_total = d3.rollup(hisp_est, v => d3.sum(v, d => d.population), d => d.year);
var nonhisp_total = d3.rollup(nonhisp_est, v => d3.sum(v, d => d.population), d => d.year, d=> d.race);

//Flattening the datasets
    var hisp_flat = [];
for (let [key, value] of hisp_total) {
  hisp_flat.push({'year' : key, 'race_eth' : 'Hispanic',  'population' : Math.round(value)});
    }
var nonhisp_flat = [];
for (let [key1, value] of nonhisp_total) {
for (let[key2, value2] of value) {
   nonhisp_flat.push({'year' : key1, 'race_eth' : key2 + ' NH', 'population' : Math.round(value2)});
}
}

var raceeth_est = hisp_flat.concat(nonhisp_flat);
var raceeth_fin = raceeth_est;

/*
raceeth_est.concat(raceeth_for).forEach(function(obj) {
    raceeth_fin.push({'year' : obj.year, 'race_eth' : obj.race_eth, 'population' : obj.population});
    });
*/

// Create table array for output
var tbl_arr = []
var race_eth_sum = d3.sum(raceeth_est, d => d.population);
var raceth = ['Hispanic', 'White alone NH', 'Black or African American alone NH',
			'Asian alone NH', 'Native Hawaiian or Other Pacific Islander alone NH', 
			'American Indian and Alaska Native alone NH', 'Two or more NH'];
			

for(i = 0; i < raceth.length; i++) {
	var filt = raceeth_fin.filter(function(d) {return d.race_eth == raceth[i]});
	//tbl_arr.push({'race_eth' : raceth[i], 'percent' : fmt_pct(filt[0].population/race_eth_sum), 'curval' : fmt_comma(filt[0].population), 'forval' : fmt_comma(filt[1].population)});
   tbl_arr.push({'race_eth' : raceth[i], 'percent' : fmt_pct(filt[0].population/race_eth_sum), 'curval' : fmt_comma(filt[0].population)});
  };


//Generate Table
d3.select('#RaceTab').html("");
$("#RaceTab" ).append( "<h2 class='h2_style'>Race/Ethnicity</h2>" );
$("#RaceTab").append("<a href='./assets/lookups/county_sya_race_lookup.html' target='_blank'>Race/Ethnicity by Single Year of Age</a>");
$("#RaceTab").append("<p></p>");

	 
var tblcolumns2 = ['Race/ Ethnicity',  'Percentage, ' + yrvalue, 'Number,  '+ yrvalue];
// Output table 

var syatab = d3.select('#RaceTab')
               .append('table')
               .style('table-layout', 'fixed');
			   
thead = syatab.append('thead');
tbody = syatab.append('tbody');
//Header
thead.append('tr')
   .selectAll('th')
   .data(tblcolumns2).enter()
   .append('th')
   .text(function(d) {return d;})
   .style("border", "1px black solid")
   .style("width", d => {d.col1 === "Race/ Ethnicity" ? "60%" : "20%"})
   .style("text-align", "center")
   .style("font-weight", "bold");
//Rows   

var rows = tbody
    .selectAll('tr')
    .data(tbl_arr).enter()
    .append('tr')
	.attr("border-spacing","0")
	.style("color","black");

//Columns
rows.append('td')
      .style("text-align", "left")
	  .style('font-size','10pt')
	  .style('width' , '60%')
	  .attr("border-spacing","0")
	  .html(function(m) { return m.race_eth; });
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .style('width' , '20%')
	  .attr("border-spacing","0")
      .html(function(m) { return m.percent; });
rows.append('td')
       .style("text-align", "right") 
	   .style('font-size','10pt')
	    .style('width' , '20%')
	   .attr("border-spacing","0")
       .html(function(m) { return m.curval; });
/*
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
      .html(function(m) { return m.forval; });
*/

}); //end of Promise

}; 
// genRaceEth

function genTenure(fips,ACSYR){
//genTenure Pulls multiple data sources from the ACS 5-year files to create final table
//Housing Tenure B25003
var fmt_pct = d3.format(".2%");
var fmt_count = d3.format(",");
var fmt_dollar = d3.format("$,");
var fmt_yr = d3.format("00");

var ACSYR5 = ACSYR - 5;

if(fips == "000") {    
	   var tenurestr_cur = genACSUrl("homepage",ACSYR, "B25003", 1, 3, "state",fips);
	} else {
      var tenurestr_cur = genACSUrl("homepage",ACSYR, "B25003", 1, 3, "county",fips);
   };
   
d3.json(tenurestr_cur).then(function(data){

//Housing Tenure Variables and Objests
 var tenure_cur = [];

//Housing Tenure Processing

    tenure_cur = tenureData(acsPrep(data),fips);

	//Calculate rank 
	var oo_val = [];
	var rental_val = [];
	for(i = 0; i < tenure_cur.length; i++){
		if(fips == "000") { 
		  oo_val.push({"fips" : tenure_cur[i].state, "count" : tenure_cur[i].oo_est, "percent" :  tenure_cur[i].oo_est_pct});
		  rental_val.push({"fips" : tenure_cur[i].state, "count" : tenure_cur[i].rent_est, "percent" :  tenure_cur[i].rent_est_pct});
		} else {
		  oo_val.push({"fips" : tenure_cur[i].county, "count" : tenure_cur[i].oo_est, "percent" :  tenure_cur[i].oo_est_pct});
		  rental_val.push({"fips" : tenure_cur[i].county, "count" : tenure_cur[i].rent_est, "percent" :  tenure_cur[i].rent_est_pct});
		}	
	}

//Building Table Array

//Selcting fips
if(fips == "000"){
	oo_tab = oo_val.filter(function(d) {return d.fips == 8;});
	rental_tab = rental_val.filter(function(d) {return d.fips == 8;});
   } else {
	oo_tab = oo_val.filter(function(d) {return d.fips == Number(fips);});
	rental_tab = rental_val.filter(function(d) {return d.fips == Number(fips);});
   };

 out_tab = oo_tab.concat(rental_tab)

var tbl_arr = [];
var censstub = "https://data.census.gov/cedsci/table?q=";

var tabno = "B25003";
var tabname = ["Owner Occupied Housing Units", "Rental Housing Units"];

if(fips == "000") {
    var censgeo = "&g=0400000US08&tid=ACSDT5Y"+ ACSYR + ".";
} else {
	var censgeo = "&g=050XX00US08"+ fips +"&tid=ACSDT5Y"+ ACSYR + ".";
};

for(i = 0; i < tabname.length;i++) {
	var topic = "<a href='" + censstub + tabno + censgeo + tabno +"' target=_blank>" + tabname[i] + "</a>"
    tbl_arr.push({'topic' : topic,
	              'count' : fmt_count(out_tab[i].count),
				  'percent' : fmt_pct(out_tab[i].percent)
                 });
	}
	

var curyr4 = ACSYR - 4;


//Generate Table
d3.select('#TenureTab').html("");
$("#TenureTab" ).append( "<h2 class='h2_style'>Housing Tenure, "+ ACSYR +"</h2>" );
$("#TenureTab").append("<a href='https://data.census.gov/' target='_blank'>American Community Survey "+ curyr4 + "-" + ACSYR + " 5-year data</a>");
$("#TenureTab").append("<p></p>");

var tblcolumns2 = ['Housing Units', 'Number', 'Percentage'];


// Output table 

var tenuretab = d3.select('#TenureTab')
               .append('table')
               .style('table-layout', 'fixed');
			   
thead = tenuretab.append('thead');
tbody = tenuretab.append('tbody');
//Header

thead.append('tr')
   .selectAll('th')
   .data(tblcolumns2).enter()
   .append('th')
   .text(function(d) {return d;})
   .style("border", "1px black solid")
   .style("width","20%")
   .style("text-align", "center")
   .style("font-weight", "bold");
   
 //Rows   

var rows = tbody
    .selectAll('tr')
    .data(tbl_arr).enter()
    .append('tr')
	.attr("width", "20%")
	.attr("border-spacing","0")
	.style("color","black");

//Columns
rows.append('td')
      .style("text-align", "left")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
	  .html(function(m) { return m.topic; });
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
      .html(function(m) { return m.count; });
rows.append('td')
       .style("text-align", "right") 
	   .style('font-size','10pt')
	   .attr("border-spacing","0")
       .html(function(m) { return m.percent; });
 }); //end of promise
}; 
// genTenure

function educData(indata,fips) {
//educData reads in the ACS Education data file and output the summary file information for home page tables
	var outdata = [];

	for(a = 0; a < indata.length; a++){ 
	 	 var temp = [];
	 if(fips == "000"){
	    temp.push({
			 'state' : indata[a].GEO1,
			 'total_est' : indata[a].B15003_001E,
			 'total_moe' : indata[a].B15003_001M,
			 'baplus_est' : indata[a].B15003_022E + indata[a].B15003_023E + indata[a].B15003_024E + indata[a].B15003_025E,
			 'baplus_moe' : Math.sqrt(Math.pow(indata[a].B15003_022M,2) + Math.pow(indata[a].B15003_023M,2) + Math.pow(indata[a].B15003_024M,2) + Math.pow(indata[a].B15003_025M,2)),
			 'baplus_est_pct' : (indata[a].B15003_022E + indata[a].B15003_023E + indata[a].B15003_024E + indata[a].B15003_025E)/indata[a].B15003_001E,
		});
	 } else {
			temp.push({
			'county' : indata[a].GEO2,
			 'total_est' : indata[a].B15003_001E,
			 'total_moe' : indata[a].B15003_001M,
			 'baplus_est' : indata[a].B15003_022E + indata[a].B15003_023E + indata[a].B15003_024E + indata[a].B15003_025E,
			 'baplus_moe' : Math.sqrt(Math.pow(indata[a].B15003_022M,2) + Math.pow(indata[a].B15003_023M,2) + Math.pow(indata[a].B15003_024M,2) + Math.pow(indata[a].B15003_025M,2)),
			 'baplus_est_pct' : (indata[a].B15003_022E + indata[a].B15003_023E + indata[a].B15003_024E + indata[a].B15003_025E)/indata[a].B15003_001E,
			 });
	 };

	 temp[0].baplus_moe_pct = acsPctMOE(temp[0].total_est,temp[0].total_moe, temp[0].baplus_est_pct,temp[0].baplus_moe);

	 outdata.push(temp);	
    }; //end of a loop

//flatten 
var outdata_flat = [];
for(i = 0; i < outdata.length; i++){
	if(fips == "000") {
		outdata_flat.push ({
			'state' : outdata[i][0].state,
			 'total_est' : outdata[i][0].total_est,
			 'total_moe' : outdata[i][0].total_moe,
			 'baplus_est' : outdata[i][0].baplus_est,
			 'baplus_moe' : outdata[i][0].baplus_moe,
			 'baplus_est_pct' : outdata[i][0].baplus_est_pct,
			 'baplus_moe_pct' : outdata[i][0].baplus_moe_pct 
		});
	} else {
		outdata_flat.push ({
			'county' : outdata[i][0].county,
			 'total_est' : outdata[i][0].total_est,
			 'total_moe' : outdata[i][0].total_moe,
			 'baplus_est' : outdata[i][0].baplus_est,
			 'baplus_moe' : outdata[i][0].baplus_moe,
			 'baplus_est_pct' : outdata[i][0].baplus_est_pct,
			 'baplus_moe_pct' : outdata[i][0].baplus_moe_pct 
		});
    };
	};
 
  return(outdata_flat);
}; 
// educData

function povData(indata,fips) {
//povData reads in the ACS Poverty data file and output the summary file information for home page table

  //extract data from indata object

	
	var outdata = [];
	for(a = 0; a < indata.length; a++){ 

	 var temp = [];
	 if(fips == "000"){
	    temp.push({
			 'state' : indata[a].GEO1,
			 'total_est' : indata[a].B06012_001E,
			 'total_moe' : indata[a].B06012_001M,
			 'pov_est' : indata[a].B06012_002E,
			 'pov_moe' : indata[a].B06012_002M,
			 'pov_est_pct' : indata[a].B06012_002E/indata[a].B06012_001E
		});
	 } else {
		temp.push({
			 'county' : indata[a].GEO2,
			 'total_est' : indata[a].B06012_001E,
			 'total_moe' : indata[a].B06012_001M,
			 'pov_est' : indata[a].B06012_002E,
			 'pov_moe' : indata[a].B06012_002M,
			 'pov_est_pct' : indata[a].B06012_002E/indata[a].B06012_001E
		 });
	 };
	 temp[0].pov_moe_pct = acsPctMOE(temp[0].total_est,temp[0].total_moe, temp[0].pov_est_pct,temp[0].pov_moe);
	 outdata.push(temp);	
    }; //end of a loop
	
	

 //flatten 
var outdata_flat = [];
for(i = 0; i < outdata.length; i++){
	if(fips == "000") {
		outdata_flat.push ({
			'state' : outdata[i][0].state,
			 'total_est' : outdata[i][0].total_est,
			 'total_moe' : outdata[i][0].total_moe,
			 'pov_est' : outdata[i][0].pov_est,
			 'pov_moe' : outdata[i][0].pov_moe,
			 'pov_est_pct' : outdata[i][0].pov_est_pct,
			 'pov_moe_pct' : outdata[i][0].pov_moe_pct 
		});
	} else {
		outdata_flat.push ({
			'county' : outdata[i][0].county,
			 'total_est' : outdata[i][0].total_est,
			 'total_moe' : outdata[i][0].total_moe,
			 'pov_est' : outdata[i][0].pov_est,
			 'pov_moe' : outdata[i][0].pov_moe,
			 'pov_est_pct' : outdata[i][0].pov_est_pct,
			 'pov_moe_pct' : outdata[i][0].pov_moe_pct 
		});
    };
	};
 
  return(outdata_flat);
}; 
// povData


function tenureData(indata,fips) {
//tenureData reads in the ACS Poverty data file and output the summary file information for home page table
	
  //extract data from indata object

	var outdata = [];
	for(a = 0; a < indata.length; a++){ 

	 var temp = [];
	 if(fips == "000"){
	    temp.push({
			 'state' : indata[a].GEO1,
			 'total_est' : indata[a].B20003_001E,
			 'total_moe' : indata[a].B25003_001M,
			 'oo_est' : indata[a].B25003_002E,
			 'oo_moe' : indata[a].B25003_002M,
			 'oo_est_pct' : indata[a].B25003_002E/indata[a].B25003_001E,
			 'rent_est' : indata[a].B25003_003E,
			 'rent_moe' : indata[a].B25003_003M,
			 'rent_est_pct' : indata[a].B25003_003E/indata[a].B25003_001E,
		});
	 } else {
		temp.push({
			 'county' : indata[a].GEO2,
			 'total_est' : indata[a].B25003_001E,
			 'total_moe' : indata[a].B25003_001M,
			 'oo_est' : indata[a].B25003_002E,
			 'oo_moe' : indata[a].B25003_002M,
			 'oo_est_pct' : indata[a].B25003_002E/indata[a].B25003_001E,
			 'rent_est' : indata[a].B25003_003E,
			 'rent_moe' : indata[a].B25003_003M,
			 'rent_est_pct' : indata[a].B25003_003E/indata[a].B25003_001E,
		 });
	 };
	
	 temp[0].oo_moe_pct = acsPctMOE(temp[0].total_est,temp[0].total_moe, temp[0].oo_est_pct,temp[0].oo_moe);
	 temp[0].rent_moe_pct = acsPctMOE(temp[0].total_est,temp[0].total_moe, temp[0].rent_est_pct,temp[0].rent_moe);

	 outdata.push(temp);	
    }; //end of a loop

 //flatten 
var outdata_flat = [];
for(i = 0; i < outdata.length; i++){
	if(fips == "000") {
		outdata_flat.push ({
			'state' : outdata[i][0].state,
			 'total_est' : outdata[i][0].total_est,
			 'total_moe' : outdata[i][0].total_moe,
			 'oo_est' : outdata[i][0].oo_est,
			 'oo_moe' : outdata[i][0].oo_moe,
			 'oo_est_pct' : outdata[i][0].oo_est_pct,
			 'oo_moe_pct' : outdata[i][0].oo_moe_pct,
			 'rent_est' : outdata[i][0].rent_est,
			 'rent_moe' : outdata[i][0].rent_moe,
			 'rent_est_pct' : outdata[i][0].rent_est_pct,
			 'rent_moe_pct' : outdata[i][0].rent_moe_pct 			 
		});
	} else {
		outdata_flat.push ({
			'county' : outdata[i][0].county,
			 'total_est' : outdata[i][0].total_est,
			 'total_moe' : outdata[i][0].total_moe,
			 'oo_est' : outdata[i][0].oo_est,
			 'oo_moe' : outdata[i][0].oo_moe,
			 'oo_est_pct' : outdata[i][0].oo_est_pct,
			 'oo_moe_pct' : outdata[i][0].oo_moe_pct,
			 'rent_est' : outdata[i][0].rent_est,
			 'rent_moe' : outdata[i][0].rent_moe,
			 'rent_est_pct' : outdata[i][0].rent_est_pct,
			 'rent_moe_pct' : outdata[i][0].rent_moe_pct 			 
	});
    };
	};
 //removing Puerto Rico and the District of Columbia
 
  return(outdata_flat);
}; 
// tenureData


function incData(indata, type, fips) {  
//incData reads in the ACS Median Household Income, Median Home Value and Median Gross Rent data file and output the summary file information on home page table
//Type: HH: Household Income, MORT: home value, RENT : gross rent

	  //extract data from indata object
	var outdata = [];
	for(a = 0; a < indata.length; a++){ 
	 	 var temp = [];
	 if(fips == "000"){
		if(type == "HH") {
	    temp.push({
			 'state' : indata[a].GEO1,
			 'inc_est' : indata[a].B19013_001E,
			 'inc_moe' : indata[a].B19013_001M
		});
		 };
		if(type == "MORT") {
	    temp.push({
			 'state' : indata[a].GEO1,
			 'inc_est' : indata[a].B25097_001E,
			 'inc_moe' : indata[a].B25097_001M
		});
		 };
		if(type == "RENT") {
	    temp.push({
			 'state' : indata[a].GEO1,
			 'inc_est' : indata[a].B25064_001E,
			 'inc_moe' : indata[a].B25064_001M
		});
		 };
	 } else {
		if(type == "HH") {
	    temp.push({
			 'county' : indata[a].GEO2,
			 'inc_est' : indata[a].B19013_001E,
			 'inc_moe' : indata[a].B19013_001M
		});
		 };
		if(type == "MORT") {
	    temp.push({
			 'county' : indata[a].GEO2,
			 'inc_est' : indata[a].B25097_001E,
			 'inc_moe' : indata[a].B25097_001M
		});
		 };
		if(type == "RENT") {
	    temp.push({
			 'county' : indata[a].GEO2,
			 'inc_est' : indata[a].B25064_001E,
			 'inc_moe' : indata[a].B25064_001M
		});
		 };
	 };
	
	 outdata.push(temp);	
    }; //end of a loop

 //flatten 
var outdata_flat = [];
for(i = 0; i < outdata.length; i++){
	if(fips == "000") {
		outdata_flat.push ({
			'state' : outdata[i][0].state,
			 'inc_est' : outdata[i][0].inc_est,
			 'inc_moe' : outdata[i][0].inc_moe
		});
	} else {
		outdata_flat.push ({
			'county' : outdata[i][0].county,
			 'inc_est' : outdata[i][0].inc_est,
			 'inc_moe' : outdata[i][0].inc_moe
		});
    };
	};
  return(outdata_flat);
}; 
// incData

//cat ACS Data Functions

//ACS Functions:
//genACSUrl  Generates ACS call from the Census API
//genCEDSCIUrl  Geneenrates data.census.gov URL
//acsPrep prepares data file for analysis, removing null valuees converting to numbers
//acsAgePct creates age percentage data from ACS inputs for Age Bar Charts
//acsAgePyr creates age percentage data from ACS inputs for Age Pyramid Charts
//acsChkDiff checks for significant increase or decrease in quantity for ACS values ACS Handbook Chapter 7
//acsPctMOE Calculates percentage MOE ACS Handbook Chapter 8 pg 6

//genACSIncome ACS data summary functions  Income
//genACSHHIncome ACS data summary functions  HH Income
//genACSEducation ACS data summary functions  Educational Attainment
//genACSRace ACS data summary functions  Race and Ethnicity
//genACSPov ACS data summary function Poverty
//acsMOE Takes square root of MOE values from summary data sets
//genACSpct generic function to calculate percentages


function genACSUrl(pgtype,acsyear, table, startidx, endidx, geotype,geolist){
//genACSUrl  Generates ACS call from the Census API
	if(geotype == "Region"){
		var geoName = 'county';
	} else {
		if(geotype == 'Municipality'){
		  var geoName = 'place';
		} else {
		  var geoName = geotype.toLowerCase();
		}
	}
	
	//ACS Call has a different structure for tables with more thn 50 vars
	var acshead = 'https://api.census.gov/data/'+ acsyear;
	var varArr = ['NAME'];
	// Geneerating lists of varibles 
	if(Array.isArray(table)) {
		for(i = 0; i < table.length; i++){	
            varArr.push(table[i]);
			}
	} else {
		for(i = startidx; i <= endidx; i++){
		 var idx3 = ('000'+i).slice(-3);
	     varArr.push(table + "_" + idx3 + "E");
	     varArr.push(table + "_" + idx3 + "M");
		}
	}
	
	var varList = varArr.toString();
//Generating final acstail	
if(endidx >= 24) {
	if(geoName == 'state'){
		if(pgtype == "profile"){
		   var acstail = '/acs/acs5?get=group(' + table +')&for=state:08&key=08fe07c2a7bf781b7771d7cccb264fe7ff8965ce';
		} else {
		   var acstail = '/acs/acs5?get=group(' + table +')&for=state:*&key=08fe07c2a7bf781b7771d7cccb264fe7ff8965ce';
		}	
	} else {
		if(pgtype == "profile"){
			var acstail = '/acs/acs5?get=group(' + table +')&for=' + geoName + ':' + geolist + '&in=state:08&key=08fe07c2a7bf781b7771d7cccb264fe7ff8965ce';
		} else {
			var acstail = '/acs/acs5?get=group(' + table +')&for=' + geoName + ':*&in=state:08&key=08fe07c2a7bf781b7771d7cccb264fe7ff8965ce';
		}
	}
	} else {
	if(geotype == 'state'){
		if(pgtype == "profile"){
		     var acstail = '/acs/acs5?get=' + varList +'&for=state:08&key=08fe07c2a7bf781b7771d7cccb264fe7ff8965ce';
		} else {
			 var acstail = '/acs/acs5?get=' + varList +'&for=state:*&key=08fe07c2a7bf781b7771d7cccb264fe7ff8965ce';
		}
	} else {
		if(pgtype == "profile"){
			var acstail = '/acs/acs5?get=' + varList +'&for=' + geoName + ':' + geolist + '&in=state:08&key=08fe07c2a7bf781b7771d7cccb264fe7ff8965ce';
		} else {
			var acstail = '/acs/acs5?get=' + varList +'&for=' + geoName + ':*&in=state:08&key=08fe07c2a7bf781b7771d7cccb264fe7ff8965ce';
		}	
	}
	}

var acsUrl = acshead + acstail;
return(acsUrl)
}; 
// genACSUrl

function genCEDSCIUrl(level,tableid, yrvalue, fipsArr) {
//genCEDSCIUrl  Geneenrates data.census.gov URL for Table source download
	var urlHead = 'https://data.census.gov/table?q=' + tableid;
	var urlTail = '&tid=ACSDT5Y' + yrvalue + '.' + tableid;
	var urlGeo = "&g=";
	    if(level == "Region") {
		if(fipsArr[0] == "08") {
			urlGeo = urlGeo + '0400000US' + fipsArr[0] + '_';
			var startVal = 1;
		} else {
			urlGeo = urlGeo + '040XX00US08_';
			var startVal = 0;
		};
		urlGeo = urlGeo + "050XX00US"
		for(i = startVal; i < fipsArr.length; i++) {
			urlGeo = urlGeo + "08" + fipsArr[i] + ",";
		}
	 urlGeo = urlGeo.slice(0, -1)
	}
	if(level == "County") {
		urlGeo = urlGeo + '040XX00US08_';
		urlGeo = urlGeo + "050XX00US" + "08" + fipsArr[0];
	};
	if(level == "Municipality") {
		var ctycode = "08" + muni_county(fipsArr[0])
		urlGeo = urlGeo + '040XX00US08_';
		urlGeo = urlGeo + "050XX00US" + ctycode + "_";
		urlGeo = urlGeo + "160XX00US08" + fipsArr[0];
	}
	
var fullUrl = urlHead + urlGeo + urlTail;

return(fullUrl)
} 
// genCEDSCIUrl


function acsPrep(inData) {
//acsPrep prepares data file for analysis, removing null values converting to numbers

//find position of name and FIPS variables 

var namePos = 0;
var statePos = 0;
var countyPos = 0;
var placePos = 0;

var name_arr = [];
for(i = 0;i < inData[0].length; i++){
	 if(inData[0][i] == 'NAME') {
		 namePos = i;
	 }
	 if(inData[0][i] == 'state') {
		 statePos = i;
	 }
	 if(inData[0][i] == 'county') {
		 countyPos = i;
	 }
	 if(inData[0][i] == 'place') {
		 placePos = i;
	 }
	 
	 if((inData[0][i].slice(-1) === "E") || (inData[0][i].slice(-1) === "M")) {
	    if(inData[0][i] != 'NAME') {
		  name_arr.push({"pos" : i, "varname" : inData[0][i]})
		};
     };	
} 

//adding name and geos to name arr
if(countyPos != 0) {
	name_arr.unshift({"pos" : countyPos, "varname" : "GEO2"});
}
if(placePos != 0) {
	name_arr.unshift({"pos" : placePos, "varname" : "GEO2"});
}
if(statePos != 0) {
	if(countyPos == 0) {
		name_arr.unshift({"pos" : countyPos, "varname" : "GEO2"});	
	}	
	name_arr.unshift({"pos" : statePos, "varname" : "GEO1"});		  
}

name_arr.unshift({"pos" : namePos, "varname" : "NAME"});

var num_data = [];
for(i = 1; i < inData.length;i++){
	  var tmp_data = inData[i];
	  var tmp_data2 = []
	  for(j = 0; j < tmp_data.length; j++){
		  if(Number(tmp_data[j])){
			  tmp_data2.push(+tmp_data[j] < 0 ? 0 : +tmp_data[j]);
		  } else {
			  tmp_data2.push(tmp_data[j]);
		  }
	  }
  num_data.push(tmp_data2);
}


var fin_data = []
for(i = 0; i < num_data.length; i++){
	var tmp_data = num_data[i];
	var tmp_name = [];
	for(j = 0; j < name_arr.length;j++){
		 var var_str = name_arr[j].varname;
		 var var_pos = name_arr[j].pos;

		 if(var_str == "GEO2" && var_pos == 0){
			 tmp_name[var_str] = 0;
		 } else {	
		 if(var_str == "NAME"){
			tmp_name[var_str] = tmp_data[var_pos].replace(", Colorado","");
		 } else{
		 tmp_name[var_str] = tmp_data[var_pos];
		 }
		 }
	}
 // Convert any strings to numbers

 var tmp_keys = Object.keys(tmp_name);
 
 for(j = 3; j < tmp_keys.length; j++){
	 var chk_key = tmp_keys[j];
	 if(typeof tmp_name[chk_key] == 'string' || tmp_name[chk_key] instanceof String) {
		 tmp_name[chk_key] = +tmp_name[chk_key];
	 }
 }
	fin_data.push(tmp_name);
}

return(fin_data);
}; 
// acsPrep



function acsAgePct(inData,fips, yrvalue, type) {
//acsAgePct creates age percentage data from ACS inputs for Age Bar Charts
	
	var numData = [];
	for(i = 0; i < inData[1].length; i++){
		numData.push(parseInt(inData[1][i]));
	}
	
	var tot_totalpopulation_e = numData[0];
	var tot_totalpopulation_m = numData[2];
	var tot_malepopulation_e  = numData[4];
	var tot_malepopulation_m = numData[6] 
	var tot_femalepopulation_e = numData[100];
	var tot_femalepopulation_m = numData[102];
	  var fips_val = parseInt(fips);
	
	if (type == "county") {
		var out_name = countyName(fips_val);
	} else {
	    var out_name = muniName(fips_val);
	};
//Output Bar chart data 
	var outData = [];

//dataArr has the addresses of column names, male, female
for(i = 0; i <= 7; i++){
    if(i == 0){ 
	       var age_cat = '0 to 4';
			var malepopulation_e = numData[8];
			var malepopulation_m  = numData[10] 
			var femalepopulation_e  = numData[104];
			var femalepopulation_m  = numData[106];
   };
	if(i == 1){
	    var age_cat = '5 to 17';
		var malepopulation_e =  numData[12] + numData[16] + numData[20];
		var malepopulation_m =  Math.sqrt(Math.pow(numData[14],2) + Math.pow(numData[18],2) + Math.pow(numData[22],2));
		var femalepopulation_e =  numData[108] + numData[112] + numData[116];
		var femalepopulation_m =  Math.sqrt(Math.pow(numData[110],2) + Math.pow(numData[114],2) + Math.pow(numData[118],2));
    };
    if(i == 2){
	    var age_cat = '18 to 24';
		var malepopulation_e =  numData[24] + numData[28] + numData[32] + numData[36];
		var malepopulation_m = Math.sqrt(Math.pow(numData[26],2) + Math.pow(numData[30],2) + Math.pow(numData[34],2) + Math.pow(numData[38],2));
		var femalepopulation_e =  numData[120] + numData[124] + numData[128] + numData[132];
		var femalepopulation_m =  Math.sqrt(Math.pow(numData[122],2) + Math.pow(numData[126],2) + Math.pow(numData[130],2) + Math.pow(numData[134],2));
		};
    if(i == 3){
	    var age_cat = '25 to 54';
		var malepopulation_e =  numData[40] + numData[44] + numData[48] + numData[52] + numData[56] + numData[60];
		var malepopulation_m =  Math.sqrt(Math.pow(numData[42],2) + Math.pow(numData[46],2) + Math.pow(numData[50],2) + Math.pow(numData[54],2) + Math.pow(numData[58],2) + Math.pow(numData[62],2));
		var femalepopulation_e =  numData[136] + numData[140] + numData[144] + numData[148] + numData[152] + numData[156];
		var femalepopulation_m =  Math.sqrt(Math.pow(numData[138],2) + Math.pow(numData[142],2) + Math.pow(numData[146],2) + Math.pow(numData[150],2) + Math.pow(numData[154],2) + Math.pow(numData[158],2));
		};
    if(i == 4){
	    var age_cat = '55 to 64';
		var malepopulation_e =   numData[64] + numData[68] + numData[72];
		var malepopulation_m =   Math.sqrt(Math.pow(numData[66],2) + Math.pow(numData[70],2) + Math.pow(numData[74],2));
		var femalepopulation_e =   numData[160] + numData[164] + numData[168];
		var femalepopulation_m =   Math.sqrt(Math.pow(numData[162],2) + Math.pow(numData[166],2) + Math.pow(numData[170],2));
		};
    if(i == 5){
	    var age_cat = '65 to 74';
		var malepopulation_e = numData[76] + numData[80] + numData[84];
		var malepopulation_m = Math.sqrt(Math.pow(numData[78],2) + Math.pow(numData[82],2) + Math.pow(numData[86],2));
		var femalepopulation_e = numData[172] + numData[176] + numData[180];
		var femalepopulation_m = Math.sqrt(Math.pow(numData[174],2) + Math.pow(numData[178],2) + Math.pow(numData[182],2));
		};
	 if(i == 6){
	    var age_cat = '75 to 84';
		var malepopulation_e = numData[88] + numData[92];
		var malepopulation_m =  Math.sqrt(Math.pow(numData[90],2) + Math.pow(numData[94],2));
		var femalepopulation_e =  numData[184] + numData[188];
		var femalepopulation_m =  Math.sqrt(Math.pow(numData[186],2) + Math.pow(numData[190],2));
		};
	 if(i == 7){
	     var age_cat = '85 +';
		var malepopulation_e =  numData[96];
		var malepopulation_m =  numData[98];
		var femalepopulation_e =  numData[192];
		var femalepopulation_m = numData[194];
		};	

	 var totalpopulation_e = malepopulation_e + femalepopulation_e;
	 var totalpopulation_m = Math.sqrt(Math.pow(malepopulation_m,2) + Math.pow(femalepopulation_m,2));
	 var pct_malepopulation_e =  malepopulation_e/tot_malepopulation_e;
	 var pct_malepopulation_m =  malepopulation_m/tot_malepopulation_e;
	 var pct_femalepopulation_e = femalepopulation_e/tot_femalepopulation_e;
	 var pct_femalepopulation_m = femalepopulation_m/tot_femalepopulation_e;
     var pct_totalpopulation_e = totalpopulation_e/tot_totalpopulation_e;
     var pct_totalpopulation_m = totalpopulation_m/tot_totalpopulation_e;
	 
	 outData.push({'fips' : fips_val, 'name' : out_name, 'year' : yrvalue, 'age_cat_no' : i, 'age_cat' : age_cat, 
	         'malepopulation_e' : malepopulation_e, 'malepopulation_m' : malepopulation_m, 
			 'tot_malepopulation_e' : tot_malepopulation_e, 'tot_malepopulation_m' : tot_malepopulation_m,
			 'pct_malepopulation_e' : pct_malepopulation_e, 'pct_malepopulation_m' : pct_malepopulation_m,
			 'femalepopulation_e' : femalepopulation_e, 'femalepopulation_m' : femalepopulation_m, 
			 'tot_femalepopulation_e' : tot_femalepopulation_e, 'tot_femalepopulation_m' : tot_femalepopulation_m,
			  'pct_femalepopulation_e' : pct_femalepopulation_e,  'pct_femalepopulation_m' : pct_femalepopulation_m,
			 'totalpopulation_e' : totalpopulation_e, 'totalpopulation_m' : totalpopulation_m,
	         'tot_totalpopulation_e' : tot_totalpopulation_e, 'tot_totalpopulation_m' : tot_totalpopulation_m,
	          'pct_totalpopulation_e' : pct_totalpopulation_e, 'pct_totalpopulation_m' : pct_totalpopulation_m});
	}; //i
	return(outData);
} 
// acsAgePct


function acsAgePyr(inData,fips, yrvalue, type) {
//acsAgePyr creates age percentage data from ACS inputs for Age Pyramid Charts
	var numData = [];
	for(i = 0; i < inData[1].length; i++){
		numData.push(parseInt(inData[1][i]));
	}
	
	var tot_totalpopulation_e = numData[0];
	var tot_totalpopulation_m = numData[2];
	var tot_malepopulation_e  = numData[4];
	var tot_malepopulation_m = numData[6] 
	var tot_femalepopulation_e = numData[100];
	var tot_femalepopulation_m = numData[102];
	  var fips_val = parseInt(fips);
	
	if (type == "county") {
		var out_name = countyName(fips_val);
	} else {
	    var out_name = muniName(fips_val);
	};
//Output Bar chart data 
	var outData = [];
    var fips_val = parseInt(fips);
	if (type == "county") {
		var out_name = countyName(fips_val);
	} else {
	    var out_name = muniName(fips_val);
	};
//Output Pyramid data
var outData = [];
for(i = 0; i <= 17;  i++){
    if(i == 0){ 
	       var age_cat = '0 to 4';
		   var malepopulation_e = numData[8];
		   var malepopulation_m = numData[10];
		   var femalepopulation_e = numData[104]
		   var femalepopulation_m = numData[106];
		   };
	if(i == 1){
	       var age_cat = '5 to 9';
		   var malepopulation_e = numData[12];
		   var malepopulation_m = numData[14];
		   var femalepopulation_e = numData[108];
		   var femalepopulation_m = numData[110];
    };
	if(i == 2){
	       var age_cat = '10 to 14';
		   var malepopulation_e = numData[16];
		   var malepopulation_m = numData[18];
		   var femalepopulation_e = numData[112];
		   var femalepopulation_m = numData[114];
    };
	if(i == 3){
	       var age_cat = '15 to 19';
		   var malepopulation_e = numData[20] + numData[24] ;
		   var malepopulation_m = Math.sqrt(Math.pow(numData[22],2) + Math.pow(numData[26],2));
		   var femalepopulation_e = numData[116] + numData[120];
		   var femalepopulation_m = Math.sqrt(Math.pow(numData[118],2) + Math.pow(numData[122],2));
    };
	if(i == 4){
	       var age_cat = '20 to 24';
		   var malepopulation_e = numData[28] + numData[32] + numData[36];
		   var malepopulation_m = Math.sqrt(Math.pow(numData[30],2) + Math.pow(numData[34],2) + Math.pow(numData[38],2));
		   var femalepopulation_e = numData[124] + numData[128] + numData[132];
		   var femalepopulation_m = Math.sqrt(Math.pow(numData[126],2) + Math.pow(numData[130],2) + Math.pow(numData[134],2));
    };
	if(i == 5){
	       var age_cat = '25 to 29';
		   var malepopulation_e = numData[40];
		   var malepopulation_m = numData[42];
		   var femalepopulation_e = numData[136];
		   var femalepopulation_m = numData[138];
    };
	if(i == 6){
	       var age_cat = '30 to 34';
		   var malepopulation_e = numData[44];
		   var malepopulation_m = numData[46];
		   var femalepopulation_e = numData[140];
		   var femailepopulation_m = numData[142];
    };
	if(i == 7){
	       var age_cat = '35 to 39';
		   var malepopulation_e = numData[48] ;
		   var malepopulation_m = numData[50];
	       var femalepopulation_e = numData[144];
	       var femalepopulation_m = numData[146];
    };
	if(i == 8){
	       var age_cat = '40 to 44';
		   var malepopulation_e = numData[52];
		   var malepopulation_m = numData[54];
		   var femalepopulation_e = numData[148];
		   var femalepopulation_m = numData[150];
    };
	if(i == 9){
	       var age_cat = '45 to 49';
		   var malepopulation_e = numData[56];
		   var malepopulation_m = numData[58];
		   var femalepopulation_e = numData[152];
		   var femalepopulation_m = numData[154]
    };
	if(i == 10){
	       var age_cat = '50 to 54';
		   var malepopulation_e = numData[60];
		   var malepopulation_m = numData[62];
		   var femalepopulation_e = numData[156];
		   var femalepopulation_m = numData[158]
    };
	if(i == 11){
	       var age_cat = '55 to 59';
		   var malepopulation_e = numData[64];
		   var malepopulation_m = numData[66];
		   var femalepopulation_e = numData[160];
		   var femalepopultrion_m = numData[164];
    };
	if(i == 12){
	       var age_cat = '60 to 64';
		   var malepopulation_e = numData[68] + numData[72];
		   var malepopulation_m = Math.sqrt(Math.pow(numData[70],2) + Math.pow(numData[74],2))
		   var femalepopulation_e = numData[164] + numData[168];
		   var femalepopulation_m = Math.sqrt(Math.pow(numData[166],2) + Math.pow(numData[170],2))
    };
	if(i == 13){
	       var age_cat = '65 to 69';
		   var malepopulation_e = numData[76] + numData[80];
		   var malepopulation_m = Math.sqrt(Math.pow(numData[78],2) + Math.pow(numData[82],2))
		   var femalepopulation_e = numData[172] + numData[176];
		   var femalepopulation_m = Math.sqrt(Math.pow(numData[174],2) + Math.pow(numData[178],2))

    };
	if(i == 14){
	       var age_cat = '70 to 74';
		   var malepopulation_e = numData[84];
		   var malepopulation_m = numData[86];
		   var femalepopulation_e = numData[180];
		   var femalepopulation_m = numData[182];
    };
	if(i == 15){
	       var age_cat = '75 to 79';
		   var malepopulation_e = numData[88];
		   var malepopulation_m = numData[90]
		   var femalepopulation_e = numData[184];
		   var femalepopulation_m = numData[186];
    };
	if(i == 16){
	       var age_cat = '80 to 84';
		   var malepopulation_e = numData[92];
		   var malepopulation_m = numData[94]
		   var femalepopulation_e = numData[188];
		   var femalepopulation_m = numData[190]
    };
	if(i == 17){
	       var age_cat = '85 +';
		   var malepopulation_e = numData[96];
		   var malepopulation_m = numData[98];
		   var femalepopulation_e = numData[192];
		   var femalepopulation_m = numData[194]
    };

	 var totalpopulation_e = malepopulation_e + femalepopulation_e;
	 var totalpopulation_m = Math.sqrt(Math.pow(malepopulation_m,2) + Math.pow(femalepopulation_m,2));
	 var pct_malepopulation_e =  malepopulation_e/tot_malepopulation_e;
	 var pct_malepopulation_m =  malepopulation_m/tot_malepopulation_e;
	 var pct_femalepopulation_e = femalepopulation_e/tot_femalepopulation_e;
	 var pct_femalepopulation_m = femalepopulation_m/tot_femalepopulation_e;
     var pct_totalpopulation_e = totalpopulation_e/tot_totalpopulation_e;
     var pct_totalpopulation_m = totalpopulation_m/tot_totalpopulation_e;
	 
	 outData.push({'fips' : fips_val, 'name' : out_name, 'year' : yrvalue, 'age_cat_no' : i, 'age_cat' : age_cat, 
	         'malepopulation_e' : malepopulation_e, 'malepopulation_m' : malepopulation_m, 
			 'tot_malepopulation_e' : tot_malepopulation_e, 'tot_malepopulation_m' : tot_malepopulation_m,
			 'pct_malepopulation_e' : pct_malepopulation_e, 'pct_malepopulation_m' : pct_malepopulation_m,
			 'femalepopulation_e' : femalepopulation_e, 'femalepopulation_m' : femalepopulation_m, 
			 'tot_femalepopulation_e' : tot_femalepopulation_e, 'tot_femalepopulation_m' : tot_femalepopulation_m,
			  'pct_femalepopulation_e' : pct_femalepopulation_e,  'pct_femalepopulation_m' : pct_femalepopulation_m,
			 'totalpopulation_e' : totalpopulation_e, 'totalpopulation_m' : totalpopulation_m,
	         'tot_totalpopulation_e' : tot_totalpopulation_e, 'tot_totalpopulation_m' : tot_totalpopulation_m,
	          'pct_totalpopulation_e' : pct_totalpopulation_e, 'pct_totalpopulation_m' : pct_totalpopulation_m});
	}; //i
	return(outData);
} 
// acsAgePyr


function acsChkDiff(curpct,curmoe, prevpct, prevmoe) {
//acsChkDiff checjs for significant increase or decrease in quantity for ACS values  ACS Handbook Chapter 7
	var se_cur = curmoe/1.645;
	var se_prev = prevmoe/1.645;
	var se_sum = Math.pow(se_cur,2) + Math.pow(se_prev,2);
	var se_sqrt = Math.sqrt(se_sum);
	var est_diff = Math.abs(curpct - prevpct);
	var diff_crit = est_diff/se_sqrt;
	
	var outcome = 'equal';
if(diff_crit > 1.645){
	if(curpct > prevpct) {
		outcome = 'increase';
	} else {
		outcome = 'decrease';
	}
}
return(outcome);
}; 
// acsChkDiff


function acsPctMOE(totest,totmoe,varpct,varmoe) {
//acsPctMOE Calculates percentage MOE ACS Handbook Chapter 8 pg 6

  var fractionY = 1/totest;
  var moeYsq =  Math.pow(totmoe,2);
  var moeXsq =  Math.pow(varmoe,2);
  var Psq = Math.pow(varpct,2);
  
  var moePsq = moeXsq - (Psq * moeYsq);
  var moeP = Math.sqrt(Math.abs(moePsq))  * fractionY;
  return(moeP)
}
// acsPctMOE


function genACSIncome(inData,type) {
//ACS Income data summary functions
var outData = [];
inData.forEach( 
 d => outData.push({ 	
		FIPS : type == 'st' ? d.GEO1 : d.GEO2,
		NAME : type == 'st' ? 'Colorado' : d.NAME,
		TOTAL_E: d.B19001_001E,
		TOTAL_M : d.B19001_001M,
		LT10K_E: d.B19001_002E,
		LT10K_M : Math.pow(d.B19001_002M,2),
		K10K19_E : d.B19001_003E + d.B19001_004E,
		K10K19_M : Math.pow(d.B19001_003M,2) + Math.pow(d.B19001_004M,2),
		K20K29_E : d.B19001_005E + d.B19001_006E,
		K20K29_M : Math.pow(d.B19001_005M,2) + Math.pow(d.B19001_006M,2),
		K30K39_E : d.B19001_007E + d.B19001_008E,
		K30K39_M : Math.pow(d.B19001_007M,2) + Math.pow(d.B19001_008M,2),
		K40K49_E : d.B19001_009E + d.B19001_010E,
		K40K49_M : Math.pow(d.B19001_009M,2) + Math.pow(d.B19001_010M,2),
		K50K59_E: d.B19001_011E,
		K50K59_M : Math.pow(d.B19001_011M,2),
		K60K74_E: d.B19001_012E,
		K60K74_M : Math.pow(d.B19001_012M,2),
		K75K99_E: d.B19001_013E,
		K75K99_M : Math.pow(d.B19001_013M,2),
		K100K124_E: d.B19001_014E,
		K100K124_M : Math.pow(d.B19001_014M,2),
		K125K149_E: d.B19001_015E,
		K125K149_M : Math.pow(d.B19001_015M,2),
		K150K199_E: d.B19001_016E,
		K150K199_M : Math.pow(d.B19001_016M,2),
		GE200K_E: d.B19001_017E,
		GE200K_M : Math.pow(d.B19001_017M,2)
	 }));
return(outData);
};
// genACSIncome

function genACSHHIncome(inData,type){
//genACSHHIncome Generates Household Income data from ACS 
var outData = [];
inData.forEach(
  d => outData.push({
		FIPS : type == 'st' ? d.GEO1 : d.GEO2,
		NAME : type == 'st' ? 'Colorado' : d.NAME,
		HH_TOTAL_E :  d.B19051_001E,
		HH_TOTAL_M : Math.pow(d.B19051_001M,2),
		HH_EARNINGS_E :  d.B19051_002E,
		HH_EARNINGS_M : Math.pow(d.B19051_002M,2),
		HH_SALARY_E :  d.B19052_002E,
		HH_SALARY_M : Math.pow(d.B19052_002M,2),
		HH_SELF_E :  d.B19053_002E,
		HH_SELF_M : Math.pow(d.B19053_002M,2),
		HH_INTEREST_E :  d.B19054_002E,
		HH_INTEREST_M : Math.pow(d.B19054_002M,2),
		HH_SOCSEC_E :  d.B19055_002E,
		HH_SOCSEC_M : Math.pow(d.B19055_002M,2),
		HH_SSI_E :  d.B19056_002E,
		HH_SSI_M : Math.pow(d.B19056_002M,2),
		HH_PUBASST_E :  d.B19057_002E,
		HH_PUBASST_M : Math.pow(d.B19057_002M,2),
		HH_SNAP_E :  d.B19058_002E,
		HH_SNAP_M : Math.pow(d.B19058_002M,2),
		HH_RETIRE_E :  d.B19059_002E,
		HH_RETIRE_M : Math.pow(d.B19059_002M,2),
		HH_OTHER_E :  d.B19060_002E,
		HH_OTHER_M : Math.pow(d.B19060_002M,2),
		TOT_INCOME_E : d.B20003_001E,
		TOT_INCOME_M : Math.pow(d.B20003_001M,2),
		TOT_EARNINGS_E :  d.B19061_001E,
		TOT_EARNINGS_M : Math.pow(d.B19061_001M,2),
		TOT_SALARY_E :  d.B19062_001E,
		TOT_SALARY_M : Math.pow(d.B19062_001M,2),
		TOT_SELF_E :  d.B19063_001E,
		TOT_SELF_M : Math.pow(d.B19063_001M,2),
		TOT_INTEREST_E :  d.B19064_001E,
		TOT_INTEREST_M : Math.pow(d.B19064_001M,2),
		TOT_SOCSEC_E :  d.B19065_001E,
		TOT_SOCSEC_M : Math.pow(d.B19065_001M,2),
		TOT_SSI_E :  d.B19066_001E,
		TOT_SSI_M : Math.pow(d.B19066_001M,2),
		TOT_PUBASST_E :  d.B19067_001E,
		TOT_PUBASST_M : Math.pow(d.B19067_001M,2),
		TOT_RETIRE_E :  d.B19069_001E,
		TOT_RETIRE_M : Math.pow(d.B19069_001M,2),
		TOT_OTHER_E :  d.B19070_001E,
		TOT_OTHER_M : Math.pow(d.B19070_001M,2)

	 }));
	 

return(outData);
} 
// genACSHHIncome


function hhincAVG(inData) {
//hhincAVG  Calculates the mean income values and moes using derived  ratios...  input data is aggregated 
	var outData = [];
	inData.forEach( d => {
		
		var HH_TOTAL_M  = Math.sqrt(d.HH_TOTAL_M);
		var HH_EARNINGS_M  = Math.sqrt(d.HH_EARNINGS_M);
		var HH_SALARY_M  = Math.sqrt(d.HH_SALARY_M);
		var HH_SELF_M  = Math.sqrt(d.HH_SELF_M);
		var HH_INTEREST_M  = Math.sqrt(d.HH_INTEREST_M);
		var HH_SOCSEC_M  = Math.sqrt(d.HH_SOCSEC_M);
		var HH_SSI_M  = Math.sqrt(d.HH_SSI_M);
		var HH_PUBASST_M  = Math.sqrt(d.HH_PUBASST_M);
		var HH_SNAP_M  = Math.sqrt(d.HH_SNAP_M);
		var HH_RETIRE_M  = Math.sqrt(d.HH_RETIRE_M);
		var HH_OTHER_M  = Math.sqrt(d.HH_OTHER_M);
		
		//Calculating the Ratio Values -- This is the ratio of households with types of earnings
		var	RAT_EARNINGS_E  = d.HH_EARNINGS_E /d.HH_TOTAL_E; 
		var	RAT_SALARY_E  = d.HH_SALARY_E /d.HH_TOTAL_E; 
		var	RAT_SELF_E  = d.HH_SELF_E /d.HH_TOTAL_E; 
		var	RAT_INTEREST_E  = d.HH_INTEREST_E /d.HH_TOTAL_E; 
		var	RAT_SOCSEC_E  = d.HH_SOCSEC_E /d.HH_TOTAL_E; 
		var	RAT_SSI_E  = d.HH_SSI_E /d.HH_TOTAL_E; 
		var	RAT_PUBASST_E  = d.HH_PUBASST_E /d.HH_TOTAL_E; 
		var RAT_SNAP_E = d.HH_SNAP_E /d.HH_TOTAL_E;
		var	RAT_RETIRE_E  = d.HH_RETIRE_E /d.HH_TOTAL_E; 
		var	RAT_OTHER_E  = d.HH_OTHER_E /d.HH_TOTAL_E; 
		
		//Calculating averages
		
		var AVG_INCOME_E = d.TOT_INCOME_E/d.HH_TOTAL_E ;
		var AVG_INCOME_M = (Math.sqrt(d.TOT_INCOME_M + (1 * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_EARNINGS_E  = d.TOT_EARNINGS_E /d.HH_EARNINGS_E ;
		var AVG_EARNINGS_M  = (Math.sqrt(d.TOT_EARNINGS_M + (RAT_EARNINGS_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_SALARY_E  = d.TOT_SALARY_E /d.HH_SALARY_E ;
		var AVG_SALARY_M  = (Math.sqrt(d.TOT_SALARY_M + (RAT_SALARY_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_SELF_E  = d.TOT_SELF_E /d.HH_SELF_E ;
		var AVG_SELF_M  = (Math.sqrt(d.TOT_SELF_M + (RAT_SELF_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_INTEREST_E  = d.TOT_INTEREST_E /d.HH_INTEREST_E ;
		var AVG_INTEREST_M  = (Math.sqrt(d.TOT_INTEREST_M + (RAT_INTEREST_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_SOCSEC_E  = d.TOT_SOCSEC_E /d.HH_SOCSEC_E ;
		var AVG_SOCSEC_M  = (Math.sqrt(d.TOT_SOCSEC_M + (RAT_SOCSEC_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_SSI_E  = d.TOT_SSI_E /d.HH_SSI_E ;
		var AVG_SSI_M  = (Math.sqrt(d.TOT_SSI_M + (RAT_SSI_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_PUBASST_E  = d.TOT_PUBASST_E /d.HH_PUBASST_E ;
		var AVG_PUBASST_M  = (Math.sqrt(d.TOT_PUBASST_M + (RAT_PUBASST_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_RETIRE_E  = d.TOT_RETIRE_E /d.HH_RETIRE_E ;
		var AVG_RETIRE_M  = (Math.sqrt(d.TOT_RETIRE_M + (RAT_RETIRE_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;
		var AVG_OTHER_E  = d.TOT_OTHER_E /d.HH_OTHER_E ;
		var AVG_OTHER_M  = (Math.sqrt(d.TOT_OTHER_M + (RAT_OTHER_E  * d.HH_TOTAL_M )))/d.HH_TOTAL_E ;

    outData.push({ FIPS : d.FIPS,
	    NAME : d.NAME,
		HH_TOTAL_E  : d.HH_TOTAL_E ,
		HH_TOTAL_M  : HH_TOTAL_M,
		HH_EARNINGS_E  : d.HH_EARNINGS_E ,
		HH_EARNINGS_M  : HH_EARNINGS_M,
		HH_SALARY_E  : d.HH_SALARY_E ,
		HH_SALARY_M  : HH_SALARY_M,
		HH_SELF_E  : d.HH_SELF_E ,
		HH_SELF_M  :  HH_SELF_M,
		HH_INTEREST_E  : d.HH_INTEREST_E ,
		HH_INTEREST_M  : HH_INTEREST_M,
		HH_SOCSEC_E  : d.HH_SOCSEC_E ,
		HH_SOCSEC_M  : HH_SOCSEC_M,
		HH_SSI_E  : d.HH_SSI_E ,
		HH_SSI_M  : HH_SSI_M,
		HH_PUBASST_E  : d.HH_PUBASST_E ,
		HH_PUBASST_M  : HH_PUBASST_M,
		HH_SNAP_E  : d.HH_SNAP_E ,
		HH_SNAP_M  : HH_SNAP_M,
		HH_RETIRE_E  : d.HH_RETIRE_E ,
		HH_RETIRE_M  : HH_RETIRE_M,
		HH_OTHER_E  : d.HH_OTHER_E ,
		HH_OTHER_M  : HH_OTHER_M,

		RAT_EARNINGS_E : RAT_EARNINGS_E,
		RAT_EARNINGS_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_EARNINGS_E,HH_EARNINGS_M),
		RAT_SALARY_E : RAT_SALARY_E,
		RAT_SALARY_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_SALARY_E,HH_SALARY_M),
		RAT_SELF_E : RAT_SELF_E,
		RAT_SELF_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_SELF_E,HH_SELF_M),
		RAT_INTEREST_E : RAT_INTEREST_E,
		RAT_INTEREST_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_INTEREST_E,HH_INTEREST_M),
		RAT_SOCSEC_E : RAT_SOCSEC_E,
		RAT_SOCSEC_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_SOCSEC_E,HH_SOCSEC_M),
		RAT_SSI_E : RAT_SSI_E,
		RAT_SSI_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_SSI_E,HH_SSI_M),
		RAT_PUBASST_E : RAT_PUBASST_E,
		RAT_PUBASST_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_PUBASST_E,HH_PUBASST_M),
		RAT_SNAP_E : RAT_SNAP_E,
		RAT_SNAP_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_SNAP_E,HH_SNAP_M),
		RAT_RETIRE_E : RAT_RETIRE_E,
		RAT_RETIRE_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_RETIRE_E,HH_RETIRE_M),
		RAT_OTHER_E : RAT_OTHER_E,
		RAT_OTHER_M : acsPctMOE(d.HH_TOTAL_E,HH_TOTAL_M,RAT_OTHER_E,HH_OTHER_M),
		
		AVG_INCOME_E  : AVG_INCOME_E ,
		AVG_INCOME_M  : AVG_INCOME_M ,
		AVG_EARNINGS_E  : AVG_EARNINGS_E ,
		AVG_EARNINGS_M  : AVG_EARNINGS_M ,
		AVG_SALARY_E  : AVG_SALARY_E ,
		AVG_SALARY_M  : AVG_SALARY_M ,
		AVG_SELF_E  : AVG_SELF_E ,
		AVG_SELF_M  : AVG_SELF_M ,
		AVG_INTEREST_E  : AVG_INTEREST_E ,
		AVG_INTEREST_M  : AVG_INTEREST_M ,
		AVG_SOCSEC_E  : AVG_SOCSEC_E ,
		AVG_SOCSEC_M  : AVG_SOCSEC_M ,
		AVG_SSI_E  : AVG_SSI_E ,
		AVG_SSI_M  : AVG_SSI_M ,
		AVG_PUBASST_E  : AVG_PUBASST_E ,
		AVG_PUBASST_M  : AVG_PUBASST_M ,
		AVG_SNAP_E : " ",
		AVG_SNAP_M : " ",
		AVG_RETIRE_E  : AVG_RETIRE_E ,
		AVG_RETIRE_M  : AVG_RETIRE_M ,
		AVG_OTHER_E  : AVG_OTHER_E ,
		AVG_OTHER_M  : AVG_OTHER_M })
	});

return(outData)
} 
// hhincAVG

function genACSEducation(inData,type) {
//genACSEducation generates ACS data for Educational attainment
var outData = [];
inData.forEach( d =>{
	  var TOTAL_E_v =   d.B15003_001E;
	  var TOTAL_M_v =  d.B15003_001M;
	  var LTHS_E_v =  d.B15003_002E + d.B15003_003E + d.B15003_004E + d.B15003_005E + d.B15003_006E + d.B15003_007E + d.B15003_008E  + 
				d.B15003_009E + d.B15003_010E + d.B15003_011E + d.B15003_012E + d.B15003_013E + d.B15003_014E + d.B15003_015E  + d.B15003_016E;
	  var LTHS_M_v =  Math.pow(d.B15003_002M,2) + Math.pow(d.B15003_003M,2) + Math.pow(d.B15003_004M,2) + Math.pow(d.B15003_005M,2) + Math.pow(d.B15003_006M,2) + Math.pow(d.B15003_007M,2) + Math.pow(d.B15003_008M,2)  + 
				Math.pow(d.B15003_009M,2) + Math.pow(d.B15003_010M,2) + Math.pow(d.B15003_011M,2) + Math.pow(d.B15003_012M,2) + Math.pow(d.B15003_013M,2) + Math.pow(d.B15003_014M,2) + Math.pow(d.B15003_015M,2)  + Math.pow(d.B15003_016M,2);
	  var HSGED_E_v  =  d.B15003_017E + d.B15003_018E;
	  var HSGED_M_v =  Math.pow(d.B15003_017M,2) + Math.pow(d.B15003_018M,2);
	  var SOMECOLL_E_v =  d.B15003_019E + d.B15003_020E;
	  var SOMECOLL_M_v =   Math.pow(d.B15003_019M,2) + Math.pow(d.B15003_020M,2);
	  var AADEG_E_v =  d.B15003_021E;
	  var AADEG_M_v = Math.pow(d.B15003_021M,2);
	  var BADEG_E_v = d.B15003_022E;
	  var BADEG_M_v = Math.pow(d.B15003_022M,2);
	  var GRADDEG_E_v =  d.B15003_023E + d.B15003_024E + d.B15003_025E;
	  var GRADDEG_M_v =  Math.pow(d.B15003_023M,2) + Math.pow(d.B15003_024M,2) + Math.pow(d.B15003_025M,2);

      outData.push({
		FIPS : type == 'st' ? d.GEO1 : d.GEO2,
		NAME : type == 'st' ? 'Colorado' : d.NAME,
	  TOTAL_E :  TOTAL_E_v,
	  TOTAL_M : TOTAL_M_v,
	  LTHS_E :  LTHS_E_v,
	  LTHS_M :  LTHS_M_v,
	  HSGED_E :  HSGED_E_v,
	  HSGED_M :  HSGED_M_v,
	  SOMECOLL_E :  SOMECOLL_E_v,
	  SOMECOLL_M :  SOMECOLL_M_v,
	  AADEG_E :  AADEG_E_v,
	  AADEG_M :  AADEG_M_v,
	  BADEG_E :  BADEG_E_v,
	  BADEG_M :  BADEG_M_v,
	  GRADDEG_E :  GRADDEG_E_v,
	  GRADDEG_M :  GRADDEG_M_v
	})
	});
  

return(outData);
} 
// genACSEducation

function genACSRace(inData,type){
//genACSRace generates rfrom race data from ACS
	var outData = [];
inData.forEach(
    d => outData.push({
		FIPS : type == 'st' ? d.GEO1 : d.GEO2,
		NAME : type == 'st' ? 'Colorado' : d.NAME,
	  TOTAL_E :  d.B03002_001E, 
	  TOTAL_M : d.B03002_001M,
	  HISP_E :  d.B03002_012E, 
	  HISP_M : Math.pow(d.B03002_012M,2),
	  NONHISP_E :  d.B03002_002E, 
	  NONHISP_M : Math.pow(d.B03002_002M,2),
	  WHITENH_E :  d.B03002_003E, 
	  WHITENH_M : Math.pow(d.B03002_003M,2),
	  BLACKNH_E :  d.B03002_004E, 
	  BLACKNH_M : Math.pow(d.B03002_004M,2),
	  AIANNH_E :  d.B03002_005E, 
	  AIANNH_M : Math.pow(d.B03002_005M,2),
	  ASIANNH_E :  d.B03002_006E, 
	  ASIANNH_M : Math.pow(d.B03002_006M,2),
	  NHPACNH_E :  d.B03002_007E, 
	  NHPACNH_M : Math.pow(d.B03002_007M,2),
	  OTHERNH_E :  d.B03002_008E, 
	  OTHERNH_M : Math.pow(d.B03002_008M,2),
	  TWONH_E :  d.B03002_009E, 
	  TWONH_M : Math.pow(d.B03002_009M,2)
  }));
 return(outData);
}  
// genACSRace


function acsMOE(inData){
//acsMOE Takes square root of MOE values from summary data sets
	  var outData = inData;
	if(Array.isArray(outData)){
	   for(i = 0; i < outData.length; i++){
			varnames = Object.keys(outData[i]);
			for(j = 0; j < varnames.length; ++j) {
				  if(varnames[j].slice(-2) == "_M" && varnames[j] !== "TOTAL_M") {
					  var sqval = outData[i][varnames[j]];
					  var sqrtval = Math.sqrt(sqval);
					  outData[i][varnames[j]] = sqrtval;
				  };
			}
	   }
	} else {
		varnames = Object.keys(outData);
		for(j = 0; j < varnames.length; ++j) {
			  if(varnames[j].slice(-2) == "_M" && varnames[j] !== "TOTAL_M") {
				  var sqval = outData[varnames[j]];
				  var sqrtval = Math.sqrt(sqval);
				  outData[varnames[j]] = sqrtval;
			  };
		}
	}
	return(outData);
} 
// acsMOE



function genACSPct(inData){
//genACSPct generic ACS function to calculate percentages
	var outData = []

	inData.forEach( d => {
		var totest = 0;
	    var totmoe = 0;
	    var pct_est_value = 0;
	    var pct_moe_value = 0;
	var varnames = Object.keys(d);

		varnames.forEach( name => {
			if(name == "TOTAL_E") {totest = d[name]};
			if(name == "TOTAL_M") {totmoe = d[name] == 0 ? 1 : d[name]};
			if(totest > 0){
				var last2chars = name.slice(-2)
				if(last2chars == "_E"){
					 pct_est_name = name + "_PCT";
					 pct_est_value = d[name]/totest;
					if(pct_est_name != "TOTAL_E_PCT"){
						d[pct_est_name] = pct_est_value;
					}
				}
			if(last2chars == "_M"){
				    var moeRaw = d[name]
					 pct_moe_name = name + "_PCT";
					 pct_moe_value = acsPctMOE(totest,totmoe,pct_est_value,moeRaw);
					if(pct_moe_name != "TOTAL_M_PCT"){
					     d[pct_moe_name] = pct_moe_value;
					}
				}
			} //totvar > 0
		}) //varnames
	outData.push(d)
	}) //inData
	

return(outData)
}  
// genACSPct


function genACS(fips,curyr){
//genACS Pulls multiple data sources from the ACS 5-year files to create final table on home page
//Poverty  B06012
//Educ ACS B15003
//Median Household income  B19013_001
//Median Home Value B25097
//Median Gross Rent B25064

var fmt_pct = d3.format(".2%");
var fmt_count = d3.format(",");
var fmt_dollar = d3.format("$,");
var fmt_yr = d3.format("00");

var prevyr = curyr - 5;

if(fips == "000") { 
       var povstr_cur = genACSUrl("homepage",curyr, "B06012", 1, 20, "state",fips);
	   var povstr_prev = genACSUrl("homepage",prevyr, "B06012", 1, 20, "state",fips);

       var educstr_cur = genACSUrl("homepage",curyr, "B15003", 1, 25, "state",fips);
	   var educstr_prev = genACSUrl("homepage",prevyr, "B15003", 1, 25, "state",fips);
	   
       var incstr_cur = genACSUrl("homepage",curyr, "B19013", 1, 1, "state",fips);
	   var incstr_prev = genACSUrl("homepage",prevyr, "B19013", 1, 1, "state",fips);
	   
	   var homestr_cur = genACSUrl("homepage",curyr, "B25097", 1, 3, "state",fips);;
	   var homestr_prev = genACSUrl("homepage",prevyr, "B25097", 1, 3, "state",fips);
	   
       var rentstr_cur = genACSUrl("homepage",curyr, "B25064", 1, 1, "state",fips);
	   var rentstr_prev = genACSUrl("homepage",curyr, "B25064", 1, 1, "state",fips);;
   } else {
       var povstr_cur = genACSUrl("homepage",curyr, "B06012", 1, 20, "county",fips);
	   var povstr_prev = genACSUrl("homepage",prevyr, "B06012", 1, 20, "county",fips);

       var educstr_cur = genACSUrl("homepage",curyr, "B15003", 1, 25, "county",fips);
	   var educstr_prev = genACSUrl("homepage",prevyr, "B15003", 1, 25, "county",fips);

       var incstr_cur = genACSUrl("homepage",curyr, "B19013", 1, 1, "county",fips);
	   var incstr_prev = genACSUrl("homepage",prevyr, "B19013", 1, 1, "county",fips);
	   
	   var homestr_cur = genACSUrl("homepage",curyr, "B25097", 1, 3, "county",fips);;
	   var homestr_prev = genACSUrl("homepage",prevyr, "B25097", 1, 3, "county",fips);
	   
       var rentstr_cur = genACSUrl("homepage",curyr, "B25064", 1, 1, "county",fips);
	   var rentstr_prev = genACSUrl("homepage",prevyr, "B25064", 1, 1, "county",fips);;
   };
   
 
  
 //Promise Structure
var prom = [d3.json(povstr_prev),d3.json(povstr_cur),d3.json(educstr_prev),d3.json(educstr_cur),
            d3.json(incstr_prev),d3.json(incstr_cur),d3.json(homestr_prev),d3.json(homestr_cur),
			d3.json(rentstr_prev),d3.json(rentstr_cur)];

Promise.all(prom).then(function(data){

//Poverty Variables and Objects
 var pov_cur = [];
 var pov_prev = [];
 var pov_comp = [];
 var pov_rank = [];
 var povRank;
 var povDiff;
 
//Education Variables and Objects
 var educ_cur = [];
 var educ_prev = [];
 var educ_comp = [];
 var educ_rank = [];
 var educRank;
 var educDiff;
 
 //Household Income Variables and Objects
 var inc_cur = [];
 var inc_prev = [];
 var inc_comp = [];
 var inc_rank = [];
 var incRank;
 var incDiff;
 
//Median Home Value Variables and Objects
 var home_cur = [];
 var home_prev = [];
 var home_comp = [];
 var home_rank = [];
 var homeRank;
 var homeDiff;
 
 //Median Gross Rent Variables and Objects
 var rent_cur = [];
 var rent_prev = [];
 var rent_comp = [];
 var rent_rank = [];
 var rentRank;
 var rentDiff;
 
//Poverty Processing


    pov_prev = povData(acsPrep(data[0]),fips);
	pov_cur = povData(acsPrep(data[1]),fips);

//Calculate rank 
   if(fips == '000') {  //Removing DC and PR
       pov_rank = pov_cur.sort(function(a, b){ return d3.ascending(b['pov_est_pct'], a['pov_est_pct']); })
	          .filter(function(d) {return d.state != 11 && d.state != 72;});
   } else {
	   pov_rank = pov_cur.sort(function(a, b){ return d3.ascending(b['pov_est_pct'], a['pov_est_pct']); });
   }

	povRank = returnRank(pov_rank,fips);
	
	//Comparing current and previous values
	if(fips == "000") {
		pov_comp = pov_cur.filter(function(d) {return d.state == 8;})
		pov_comp.push(pov_prev.filter(function(d) {return d.state == 8;}));
	} else {
		pov_comp = pov_cur.filter(function(d) {return d.county == parseInt(fips);})
		pov_comp.push(pov_prev.filter(function(d) {return d.county == parseInt(fips);}));
    };

	povDiff = acsChkDiff(pov_comp[0]['pov_est_pct'], pov_comp[0]['pov_moe_pct'], pov_comp[1][0]['pov_est_pct'], pov_comp[1][0]['pov_moe_pct']);

//Education Processing

    educ_prev = educData(acsPrep(data[2]),fips);
	educ_cur = educData(acsPrep(data[3]),fips);

 
	//Calculate rank 
	if(fips == '000') {  //Removing DC and PR
	  educ_rank = educ_cur.sort(function(a, b){ return d3.ascending(b['baplus_est_pct'], a['baplus_est_pct']); })
	           .filter(function(d) {return d.state != 11 && d.state != 72;});
	} else {
	  educ_rank = educ_cur.sort(function(a, b){ return d3.ascending(b['baplus_est_pct'], a['baplus_est_pct']); });
   }
	educRank = returnRank(educ_rank,fips);
	
//Comparing current and previous values
	if(fips == "000") {
		educ_comp = educ_cur.filter(function(d) {return d.state == 8;})
		educ_comp.push(educ_prev.filter(function(d) {return d.state == 8;}));
	} else {
		educ_comp = educ_cur.filter(function(d) {return d.county == parseInt(fips);})
		educ_comp.push(educ_prev.filter(function(d) {return d.county == parseInt(fips);}));
    };
	educDiff = acsChkDiff(educ_comp[0]['baplus_est_pct'], educ_comp[0]['baplus_moe_pct'], educ_comp[1][0]['baplus_est_pct'], educ_comp[1][0]['baplus_moe_pct']);

//Median Household Income Processing

    inc_prev = incData(acsPrep(data[4]),"HH",fips);
	inc_cur = incData(acsPrep(data[5]),"HH",fips);

	
	//Calculate rank 
	 if(fips == '000') {  //Removing DC and PR
       inc_rank = inc_cur.sort(function(a, b){ return d3.ascending(b['inc_est'], a['inc_est']); })
	          .filter(function(d) {return d.state != 11 && d.state != 72;});
   } else {
	   inc_rank = inc_cur.sort(function(a, b){ return d3.ascending(b['inc_est'], a['inc_est']); });
   }
	incRank = returnRank(inc_rank,fips);
	
//Comparing current and previous values
	if(fips == "000") {
		inc_comp = inc_cur.filter(function(d) {return d.state == 8;})
		inc_comp.push(inc_prev.filter(function(d) {return d.state == 8;}));
	} else {
		inc_comp = inc_cur.filter(function(d) {return d.county == parseInt(fips);})
		inc_comp.push(inc_prev.filter(function(d) {return d.county == parseInt(fips);}));
    };
	incDiff = acsChkDiff(inc_comp[0]['inc_est'], inc_comp[0]['inc_moe'], inc_comp[1][0]['inc_est'], inc_comp[1][0]['inc_moe']);
	
//Median Home Value  Processing
	
    home_prev = incData(acsPrep(data[6]),"MORT",fips);
	home_cur = incData(acsPrep(data[7]),"MORT",fips);
	
	//Calculate rank 
	 if(fips == '000') {  //Removing DC and PR
       home_rank = home_cur.sort(function(a, b){ return d3.ascending(b['inc_est'], a['inc_est']); })
	          .filter(function(d) {return d.state != 11 && d.state != 72;});
   } else {
	   home_rank = home_cur.sort(function(a, b){ return d3.ascending(b['inc_est'], a['inc_est']); });
   }
	homeRank = returnRank(home_rank,fips);
	
//Comparing current and previous values
	if(fips == "000") {
		home_comp = home_cur.filter(function(d) {return d.state == 8;})
		home_comp.push(home_prev.filter(function(d) {return d.state == 8;}));
	} else {
		home_comp = home_cur.filter(function(d) {return d.county == parseInt(fips);})
		home_comp.push(home_prev.filter(function(d) {return d.county == parseInt(fips);}));
    };
	homeDiff = acsChkDiff(home_comp[0]['inc_est'], home_comp[0]['inc_moe'], home_comp[1][0]['inc_est'], home_comp[1][0]['inc_moe']);
	
//Median Gross Rent Processing


    rent_prev = incData(acsPrep(data[8]),"RENT",fips);
	rent_cur = incData(acsPrep(data[9]),"RENT",fips);

	//Calculate rank 
	 if(fips == '000') {  //Removing DC and PR
	 var rent_tmp = rent_cur.filter(function(d) {return d.state != 11 && d.state != 72;});
     var  rent_rank = rent_tmp.sort(function(a, b){ return parseInt(b.inc_est) - parseInt(a.inc_est); })        
   } else {
	   rent_rank = rent_cur.sort(function(a, b){ return d3.ascending(b['inc_est'], a['inc_est']); });
   }
 
	rentRank = returnRank(rent_rank,fips);
	
//Comparing current and previous values
	if(fips == "000") {
		rent_comp = rent_cur.filter(function(d) {return d.state == 8;})
		rent_comp.push(rent_prev.filter(function(d) {return d.state == 8;}));
	} else {
		rent_comp = rent_cur.filter(function(d) {return d.county == parseInt(fips);})
		rent_comp.push(rent_prev.filter(function(d) {return d.county == parseInt(fips);}));
    };
	rentDiff = acsChkDiff(rent_comp[0]['inc_est'], rent_comp[0]['inc_moe'], rent_comp[1][0]['inc_est'], rent_comp[1][0]['inc_moe']);
	
//Building Table Array
var tbl_arr = [];
var censstub = "https://data.census.gov/cedsci/table?q=";

var tabno = ["B06012","B15003","B19013","B25097","B25064"];
var tabname = ["% living in Poverty","% with Bachelor's Degree+",
              "Median Household Income", "Median Home Value", "Median Gross Rent"];

if(fips == "000") {
    var censgeo = "&g=0400000US08&tid=ACSDT5Y" + curyr + ".";
} else {
	var censgeo = "&g=050XX00US08"+ fips +"&tid=ACSDT5Y" + curyr + ".";
};


for(i = 0; i < 5; i++){
	var topic = "<a href='" + censstub + tabno[i] + censgeo + tabno[i] +"' target=_blank>" + tabname[i] + "</a>"
	if(i == 0) {//poverty
	  var value = fmt_pct(pov_comp[0]['pov_est_pct']);
	  if(povDiff == "increase"){
		 var diffIcon = "<i class='fas fa-arrow-up' style='color: red;'></i>";
	  };
	  if(povDiff == "decrease"){
		 var diffIcon = "<i class='fas fa-arrow-down' style='color: green;'></i>";
	  };
	  if(povDiff == "equal") {
		  var diffIcon = "<i class='fas fa-equals' style='color: grey;'></i>";
	  };
	 var rank = povRank;
	}; 

	if(i == 1) {//education
	  var value = fmt_pct(educ_comp[0]['baplus_est_pct']);
	  if(educDiff == "increase"){
		 var diffIcon = "<i class='fas fa-arrow-up' style='color: green;'></i>";
	  };
	  if(educDiff == "decrease"){
		 var diffIcon = "<i class='fas fa-arrow-down' style='color: red;'></i>";
	  };
	  if(educDiff == "equal") {
		  var diffIcon = "<i class='fas fa-equals' style='color: grey;'></i>";
	  };
	 var rank = educRank;
	}; 

	if(i == 2) {//Household Income
	  var value = fmt_dollar(inc_comp[0]['inc_est']);
	  if(incDiff == "increase"){
		 var diffIcon = "<i class='fas fa-arrow-up' style='color: green;'></i>";
	  };
	  if(incDiff == "decrease"){
		 var diffIcon = "<i class='fas fa-arrow-down' style='color: red;'></i>";
	  };
	  if(incDiff == "equal") {
		  var diffIcon = "<i class='fas fa-equals' style='color: grey;'></i>";
	  };
	 var rank = incRank;
	}; 

	if(i == 3) {//homevalue
	  var value = fmt_dollar(home_comp[0]['inc_est']);
	  if(homeDiff == "increase"){
		 var diffIcon = "<i class='fas fa-arrow-up' style='color: green;'></i>";
	  };
	  if(homeDiff == "decrease"){
		 var diffIcon = "<i class='fas fa-arrow-down' style='color: red;'></i>";
	  };
	  if(homeDiff == "equal") {
		  var diffIcon = "<i class='fas fa-equals' style='color: grey;'></i>";
	  };
	 var rank = homeRank;
	}; 
	
	if(i == 4) {//gross rent
	  var value = fmt_dollar(rent_comp[0]['inc_est']);
	  if(rentDiff == "increase"){
		 var diffIcon = "<i class='fas fa-arrow-up' style='color: green;'></i>";
	  };
	  if(rentDiff == "decrease"){
		 var diffIcon = "<i class='fas fa-arrow-down' style='color: red;'></i>";
	  };
	  if(rentDiff == "equal") {
		  var diffIcon = "<i class='fas fa-equals' style='color: grey;'></i>";
	  };
	 var rank = rentRank;
	}; 
	 
	tbl_arr.push({ 'topic' : topic,
	               'value' : value, 
	               'diff' : diffIcon,
				   'rank' : rank});
}; //tbl_arr i loop

var curyr4 = curyr - 4;
var prevyr4 = prevyr - 4;

//Generate Table
d3.select('#ACSTab').html("");
$("#ACSTab" ).append( "<h2 class='h2_style'>Selected Statistics, "+curyr+"</h2>" );
$("#ACSTab").append("<a href='https://data.census.gov/' target='_blank'>American Community Survey "+ curyr4 + "-" + curyr + " 5-year data</a>");
$("#ACSTab").append("<p></p>");

if(fips == "000"){	
     var tblcolumns2 = ['Topic', 'Number', 'Change from ' + prevyr4 + "-" + prevyr + " ACS","U. S. Rank"];
} else {
	 var tblcolumns2 = ['Topic', 'Value', 'Change from ' + prevyr4 + "-" + prevyr + " ACS","County Rank"];
};

// Output table 

var syatab = d3.select('#ACSTab')
               .append('table')
               .style('table-layout', 'fixed');
			   
thead = syatab.append('thead');
tbody = syatab.append('tbody');
//Header
thead.append('tr')
   .selectAll('th')
   .data(tblcolumns2).enter()
   .append('th')
   .text(function(d) {return d;})
   .style("border", "1px black solid")
   .style("width","20%")
   .style("text-align", "center")
   .style("font-weight", "bold");
   
 //Rows   

var rows = tbody
    .selectAll('tr')
    .data(tbl_arr).enter()
    .append('tr')
	.attr("width", "20%")
	.attr("border-spacing","0")
	.style("color","black");

//Columns
rows.append('td')
      .style("text-align", "left")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
	  .html(function(m) { return m.topic; });
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
      .html(function(m) { return m.value; });
rows.append('td')
       .style("text-align", "right") 
	   .style('font-size','10pt')
	   .attr("border-spacing","0")
       .html(function(m) { return m.diff; });
 rows.append('td')
       .style("text-align", "right") 
	   .style('font-size','10pt')
	   .attr("border-spacing","0")
       .html(function(m) { return m.rank; });
 }); //end of promise
}; 
// genACS



function genHousing(fips, yrvalue) {
//genHousing generates housing data for home Page table From the SDO County Profile
	var fmt_pct = d3.format(".2%")
	var fmt_comma = d3.format(",");
	var fmt_dec = d3.format(".2f");


//Specify fips_list
var fips_list = parseInt(fips); 
   
//extract year value 
  
  var prevyear = yrvalue - 1;
  var yr_list = prevyear + "," + yrvalue;
 
 //Generate url
 if(fips == "000") {
 fips_list = "1,3,5,7,9,11,13,14,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125";
 } 
//estimates urls
urlstr = "https://gis.dola.colorado.gov/lookups/profile?county=" + fips_list + "&year=" + yr_list + "&vars=totalhousingunits,householdpopulation,groupquarterspopulation,censusbuildingpermits,hhldpoptothuratio";

var hous_tmp = [];
d3.json(urlstr).then(function(data){

 data.forEach(function(obj) {
 hous_tmp.push({'year' : obj.year,  
     'county' : obj.county,
     'totalhousingunits' : parseInt(obj.totalhousingunits),
     'householdpopulation' : parseInt(obj.householdpopulation),
	 'groupquarterspopulation' : parseInt(obj.groupquarterspopulation),
	 'censusbuildingpermits' : parseInt(obj.censusbuildingpermits),
	 'hhldpoptothuratio' : parseInt(obj.hhldpoptothuratio)
 });
 });
 
var columnsToSum = ['totalhousingunits','householdpopulation','groupquarterspopulation', 'censusbuildingpermits']
 
//  Totals
var housing_temp = d3.rollup(hous_tmp,
                  v => Object.fromEntries(columnsToSum.map(col => [col, d3.sum(v, d => +d[col])])), d => d.year);

var housing_fin = [];
for (let [key, obj] of housing_temp) {
	housing_fin.push({'year' : key,  
                        'totalhousingunits' : obj.totalhousingunits,
                        'householdpopulation' : obj.householdpopulation,
	                    'groupquarterspopulation' : obj.groupquarterspopulation,
	                    'censusbuildingpermits' : obj.censusbuildingpermits,
						'hhldpoptothuratio' : obj.householdpopulation/obj.totalhousingunits
 });
};

var housing_T = transpose(housing_fin);

var housing_fint = Object.values(housing_T);


var tbl_arr = [];
var out_name  = "";
var currentVal = 0;
var prevVal = 0;
var cVal = 0;
var pctVal = 0;

for(i = 1; i < housing_fint.length; i++){
	currentVal = housing_fint[i][1][0]['value'];
	prevVal = housing_fint[i][0][0]['value']; 
	
	if(housing_fint[i][0][0].name == 'totalhousingunits') { out_name = "Total Housing Units";
	                                                     cVal = fmt_comma(currentVal);
														 pctVal = fmt_pct((currentVal-prevVal)/prevVal);
													   };
	if(housing_fint[i][0][0].name == 'householdpopulation') { out_name = "Household Population";
	                                                     cVal = fmt_comma(currentVal);
														 pctVal = fmt_pct((currentVal - prevVal)/prevVal);
													   };
	if(housing_fint[i][0][0].name == 'groupquarterspopulation') { out_name = "Group Quarters Population";
	                                                     cVal = fmt_comma(currentVal);
														 pctVal = fmt_pct((currentVal - prevVal)/prevVal);
													   };
	if(housing_fint[i][0][0].name == 'censusbuildingpermits') { out_name = "Annual Building Permits";
	                                                     cVal = fmt_comma(currentVal);
														 if(prevVal != 0) {
														   pctVal = fmt_pct((currentVal - prevVal)/prevVal);
														 } else {
														   pctVal = " ";
														 };
	}
	if(housing_fint[i][0][0].name == 'hhldpoptothuratio') { out_name = " Household Population to Housing Unit Ratio";
	                                                     cVal = fmt_dec(currentVal);
														 pctVal = " ";
														 };

	tbl_arr.push({ 'row_name' : out_name,
	               'curval' : cVal, 
	               'pct_chg' : pctVal});
};


//Generate Table
d3.select('#HousTab').html("");
$("#HousTab" ).append( "<h2 class='h2_style'>Housing Characteristics, "+yrvalue+"</h2>" );
$("#HousTab").append("<a href='./assets/lookups/county_pop_lookup.html' target='_blank'>County Population and Housing Lookup</a>" );
$("#HousTab").append("<p></p>");

var tblcolumns2 = ['Housing Type', 'Number', 'Change from ' + prevyear];
// Output table 

var syatab = d3.select('#HousTab')
               .append('table')
               .style('table-layout', 'fixed');
			   
thead = syatab.append('thead');
tbody = syatab.append('tbody');
//Header
thead.append('tr')
   .selectAll('th')
   .data(tblcolumns2).enter()
   .append('th')
   .text(function(d) {return d;})
   .style("border", "1px black solid")
   .style("width","33%")
   .style("text-align", "center")
   .style("font-weight", "bold");
//Rows   

var rows = tbody
    .selectAll('tr')
    .data(tbl_arr).enter()
    .append('tr')
	.attr("width", "33%")
	.attr("border-spacing","0")
	.style("color","black");

//Columns
rows.append('td')
      .style("text-align", "left")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
	  .html(function(m) { return m.row_name; });
rows.append('td')
      .style("text-align", "right")
	  .style('font-size','10pt')
	  .attr("border-spacing","0")
      .html(function(m) { return m.curval; });
rows.append('td')
       .style("text-align", "right") 
	   .style('font-size','10pt')
	   .attr("border-spacing","0")
       .html(function(m) { return m.pct_chg; });

}); //d3.json
}; 
//  genHousing

//cat Demographic Dashboard Functions

function estPlot(inData, app, level, plotdiv, bkmark, yrvalue, fips, ctyName){
//estPlot Component Functions for Demograpic Dashboard : Estimates Plot
    const fmt_date = d3.timeFormat("%B %d, %Y");

var config = {responsive: true,
              displayModeBar: false};
//Clearing out divs

if(app == "profile") {
	  pgSetupPro(level,"chart",plotdiv,bkmark,true,false,fips, ctyName, 0)
	  var ESTIMATE = document.getElementById('PlotDiv2');
} else {
	var ESTIMATE = document.getElementById(plotdiv);
}

ESTIMATE.innerHTML = "";
//Prepping Plot

var year_est_arr =[];
var pop_est_arr = [];

est_flat = inData.sort(function(a, b){ return d3.ascending(a['year'], b['year']); });
year_est_arr = est_flat.map(item => item.year);
pop_est_arr = est_flat.map(item => item.totalpopulation);
if(app == "dashboard"){
var est_trace = { 
               x: year_est_arr,
               y : pop_est_arr,
			   type : 'bar'
			};
} else {
	var est_trace = { 
               x: year_est_arr,
               y : pop_est_arr,
			   mode: 'lines+markers'
	}
}
var est_data = [est_trace];
var est_layout = {
		title: "Population Estimates 1985 to "+ yrvalue + ", " + ctyName,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			title : 'Total Population',
			automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
		
Plotly.newPlot(ESTIMATE, est_data, est_layout,config);

if(app == 'profile'){
var profileDat2 = document.getElementById('profileDat2');
var profileImg2 = document.getElementById('profileImg2');
profileDat2.onclick = function() {exportToCsv(ctyName, 'estimate', est_flat,0)};
profileImg2.onclick = function() {exportToPng(ctyName, 'estimate', ESTIMATE,0)};
} else { //These are the dashboard events
var est_csv = document.getElementById('est_csv');
var est_png = document.getElementById('est_png');
est_csv.onclick = function() {exportToCsv(ctyName, 'estimate', est_flat,0)};
est_png.onclick = function() {exportToPng(ctyName, 'estimate', ESTIMATE,0)};
}	//profile
}; 
// estPlot


function forecastPlot(inData, app, level, plotdiv, bkmark, yrvalue,fips,ctyName) {
//forecastPlot Component Functions for Demograpic Dashboard: Forecast Plot
    const fmt_date = d3.timeFormat("%B %d, %Y");
	const fmt_pct = d3.format(".0%");
	const fmt_pct1 = d3.format(".1%");
	const fmt_comma = d3.format(",");
	var config = {responsive: true,
              displayModeBar: false};
			  
if(app == "profile") {
	  pgSetupPro(level,"chart",plotdiv,bkmark,true,false,fips, ctyName, 0)
	  var FORECAST = document.getElementById('PlotDiv3');
} else {
	var FORECAST = document.getElementById(plotdiv);
}
FORECAST.innerHTML = "";

//Population Projections  This data is a single Geo by age...
//Rollup
	var columnsFor = ['totalpopulation'];
	var forecast_sum = d3.rollup(inData, v => Object.fromEntries(columnsFor.map(col => [col, d3.sum(v, d => +d[col])])), d => d.year);

	var forecast_data = [];
		for (let[key, value] of forecast_sum) {
		   forecast_data.push({'fips' : fips, 'name' : ctyName, 'year' :  key, 'totalpopulation' : value.totalpopulation});
		}

var year_forec_arr =[];
var pop_forec_arr = [];

forec_flat = forecast_data.sort(function(a, b){ return d3.ascending(a['year'], b['year']); });
year_forec_arr = forec_flat.map(item => item.year);
pop_forec_arr = forec_flat.map(item => item.totalpopulation);

if(app == 'dashboard'){
var forec_trace = { 
               x: year_forec_arr,
               y : pop_forec_arr,
			   type : 'bar'
			};
} else {
var forec_trace = { 
               x: year_forec_arr,
               y : pop_forec_arr,
			   mode : 'lines+markers'
			};
}
	
var forec_tr = [forec_trace];

var forec_layout = {
		title: "Population Projections 2010 to 2050, " + ctyName ,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: false,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			title : 'Total Population',
			automargin: true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(FORECAST, forec_tr, forec_layout,config);
//Data formatting

var forecast_shift  = [];
forecast_data.forEach(item => {
    forecast_shift.push({'fips' : fips, 'name' : ctyName, 'year' :  item.year, 'totalpopulation' : fmt_comma(item.totalpopulation)});
});


//Forecasts
var forecast_names = {
	       fips : 'FIPS',
		   name : 'Location',
		   year : 'Year',
		   totalpopulation : "Population"
         };
		 
var forecast_fin = changeKeyObjects(forecast_shift,forecast_names);

//Data and plot output 
if(app == 'profile'){
	var profileDat2 = document.getElementById('profileDat3');
	var profileImg2 = document.getElementById('profileImg3');
	profileDat2.onclick = function() {exportToCsv(ctyName, 'forecast', forecast_fin,0)};
	profileImg2.onclick = function() {exportToPng(ctyName, 'forecast', FORECAST,0)};
} else { //These are the dashboard events
	var est_csv = document.getElementById('forec_csv');
	var est_png = document.getElementById('forec_png');
	est_csv.onclick = function() {exportToCsv(ctyName, 'forecast', forecast_fin,0)};
	est_png.onclick = function() {exportToPng(ctyName, 'forecast', FORECAST,0)};
}	//profile

	
}; 
// forecastPlot



function agePlot (inData, app, plotdiv,yrvalue,fips,ctyName) {
//agePlot Component Functions for Demograpic Dashboard: Age Plot 
	    const fmt_date = d3.timeFormat("%B %d, %Y");
	const fmt_pct = d3.format(".0%");
	const fmt_pct1 = d3.format(".1%");
	const fmt_comma = d3.format(",");
	var config = {responsive: true,
              displayModeBar: false};
	//Output for Age Plot (Current year by age categories...
var AGEPLOT = document.getElementById(plotdiv);

AGEPLOT.innerHTML = "";


var popName0 = "Population_" + yrvalue;

var agedata =  inData.filter(d => d.year == yrvalue);
var totaldata = [];
agedata.forEach(function(obj) {
    if(obj.age >=  0 && obj.age <= 17) {obj.age_cat = "0 to 17"; }
    if(obj.age >= 18 && obj.age <= 24) {obj.age_cat = "18 to 24";}
    if(obj.age >= 25 && obj.age <= 44) {obj.age_cat = "25 to 44"; }
    if(obj.age >= 45 && obj.age <= 54) {obj.age_cat = "45 to 54"; }
    if(obj.age >= 55 && obj.age <= 64) {obj.age_cat = "55 to 64"; }
	if(obj.age >= 65 && obj.age <= 74) {obj.age_cat = "65 to 74"; }
	if(obj.age >= 75 && obj.age <= 84) {obj.age_cat = "75 to 84"; }
    if(obj.age >= 85 ) {obj.age_cat = "85 +";}
    totaldata.push({'year' : obj.year, 'age_cat' : obj.age_cat, 'totalpopulation' : obj.totalpopulation});
});

//  Totals
var total_ann = d3.rollup(totaldata, v => d3.sum(v, d => d.totalpopulation), d => d.year);
var total_age = d3.rollup(totaldata, v => d3.sum(v, d => d.totalpopulation), d => d.year, d => d.age_cat);


//Flatten Arrays for output
var total_ann_flat = [];
for (let [key, value] of total_ann) {
  total_ann_flat.push({'year' : key, 'totalpopulation' : value});
    };

var age_idx;
var total_age_flat = [];
for (let [key1, value] of total_age) {
for (let[key2, value2] of value) {
  //Setting index value
   if(key2 == "85 +") {age_idx = 7};
   if(key2 == "0 to 17") {age_idx = 0};
   if(key2 == "18 to 24") {age_idx = 1};
   if(key2 == "25 to 44") {age_idx = 2};
   if(key2 == "45 to 54") {age_idx = 3};
   if(key2 == "55 to 64") {age_idx = 4};
   if(key2 == "65 to 74") {age_idx = 5};
   if(key2 == "75 to 84") {age_idx = 6};
  
   total_age_flat.push({'year' : key1, 'index' : age_idx, 'age_cat' : key2, 'totalpopulation' : value2});
}
}

//calculate percentages for age plot
var agep = total_age_flat.filter(d => d.year == yrvalue);
var agetotal = total_ann_flat.filter(d => d.year == yrvalue);
var ageplot = [];
agep.forEach( function(d) {
      ageplot.push({'index' : d.index, 'age_cat' : d.age_cat, 'totalpopulation' : d.totalpopulation, 'percent' : d.totalpopulation/agetotal[0].totalpopulation});
});


var age_arr =[];
var pct_arr = [];


//generate the plot
ageplot_flat = ageplot.sort(function(a, b){ return d3.ascending(a['index'], b['index']); });
age_arr = ageplot_flat.map(item => item.age_cat);
pct_arr = ageplot_flat.map(item => item.percent);

//Getting data from page

var age_trace = { 
               x: pct_arr,
               y : age_arr,
			   type : 'bar',
			   orientation : 'h'
			};

var age_data = [age_trace];

var age_layout = {
		title: "Population by Age Group " + yrvalue + ", " + ctyName,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Percentage',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			tickformat: ',.0%'
		  },
		  yaxis: {
            autorange : 'reversed',
			automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		}; 
		
Plotly.newPlot(AGEPLOT, age_data, age_layout,config);

 var agedata_names = {
	        fips : "FIPS",
			loc : "Location",
	        age_cat : "Age Group",
			totalpopulation : "Total Population " + yrvalue,
            percent : "Percent"
			};
 
var agedata_shift =[];
ageplot_flat.forEach( d => {
	agedata_shift.push({ fips : fips,
	                     loc : ctyName,
	                     age_cat : d.age_cat,
	                     totalpopulation : fmt_comma(d.totalpopulation),
	                     percent : fmt_pct1(d.percent) 
						 });
});

 var agedata_out  = changeKeyObjects(agedata_shift, agedata_names);

var age_csv = document.getElementById('age_csv');
var age_png = document.getElementById('age_png');
age_csv.onclick = function() {exportToCsv(ctyName, 'age', agedata_out,0) }; 
age_png.onclick = function() {exportToPng(ctyName, 'age', AGEPLOT,0)};
} 
// agePlot

function popchngPlot(inData, app, unit, plotdiv,yrvalue,fips,ctyName) {
//popchngPlot Component Functions for Demograpic Dashboard: Population Change Plot 
	const fmt_date = d3.timeFormat("%B %d, %Y");
	const fmt_pct = d3.format(".0%");
	const fmt_pct1 = d3.format(".1%");
	const fmt_comma = d3.format(",");
	var config = {responsive: true,
              displayModeBar: false};
//10-year change plot...

var POPCHNG = document.getElementById(plotdiv);
POPCHNG.innerHTML = "";

var yr10 = yrvalue + 10;

var agedata = inData.filter(d => d.year == yrvalue || d.year == yr10);

// Data
var totaldata = [];
agedata.forEach(function(obj) {
    if(obj.age >=  0 && obj.age <= 17) {obj.age_cat = "0 to 17"; }
    if(obj.age >= 18 && obj.age <= 24) {obj.age_cat = "18 to 24";}
    if(obj.age >= 25 && obj.age <= 44) {obj.age_cat = "25 to 44"; }
    if(obj.age >= 45 && obj.age <= 54) {obj.age_cat = "45 to 54"; }
    if(obj.age >= 55 && obj.age <= 64) {obj.age_cat = "55 to 64"; }
	if(obj.age >= 65 && obj.age <= 74) {obj.age_cat = "65 to 74"; }
	if(obj.age >= 75 && obj.age <= 84) {obj.age_cat = "75 to 84"; }
    if(obj.age >= 85 ) {obj.age_cat = "85 +";}
    totaldata.push({'year' : obj.year, 'age_cat' : obj.age_cat, 'totalpopulation' : obj.totalpopulation});
});

//  Totals
var total_ann = d3.rollup(totaldata, v => d3.sum(v, d => d.totalpopulation), d => d.year);
var total_age = d3.rollup(totaldata, v => d3.sum(v, d => d.totalpopulation), d => d.year, d => d.age_cat);


//Flatten Arrays for output
var total_ann_flat = [];
for (let [key, value] of total_ann) {
  total_ann_flat.push({'year' : key, 'index' : 0, 'age_cat' : 'Total', 'totalpopulation' : value});
     };

var age_idx;
var total_age_flat = [];
for (let [key1, value] of total_age) {
for (let[key2, value2] of value) {
  //Setting index value
   if(key2 == "0 to 17") {age_idx = 1};
   if(key2 == "18 to 24") {age_idx = 2};
   if(key2 == "25 to 44") {age_idx = 3};
   if(key2 == "45 to 54") {age_idx = 4};
   if(key2 == "55 to 64") {age_idx = 5};
   if(key2 == "65 to 74") {age_idx = 6};
   if(key2 == "75 to 84") {age_idx = 7};
   if(key2 == "85 +") {age_idx = 8};

   total_age_flat.push({'year' : key1, 'index' : age_idx, 'age_cat' : key2, 'totalpopulation' : value2});
}
}
total_age_flat = total_ann_flat.concat(total_age_flat).sort(function(a, b){ return d3.ascending(a['index'], b['index']); });

var agep0 = total_age_flat.filter(d => d.year == yrvalue);
var agep1 = total_age_flat.filter(d => d.year == yr10);

var popchng = [];
for(i = 0; i < agep0.length; i++){
    popchng.push({ 'index' : agep0[i].index, 'age_cat' : agep0[i].age_cat, 'p0' : agep0[i].totalpopulation, 'p1' : agep1[i].totalpopulation, 
	'popchng' : (agep1[i].totalpopulation - agep0[i].totalpopulation),
	'pctchng' : ((agep1[i].totalpopulation - agep0[i].totalpopulation)/agep0[i].totalpopulation) });
}

//Plotting
var popchng_pct_arr = [];
var popchng_pct_fmt = [];
var popchng_pop_arr = [];
var popchng_pop_fmt = [];
var popchng_cat_arr = [];

popchng_flat = popchng.sort(function(a, b){ return d3.ascending(a['index'], b['index']); });
popchng_cat_arr = popchng_flat.map(item => item.age_cat);
popchng_pop_arr = popchng_flat.map(item => item.popchng);
popchng_pop_fmt = popchng_flat.map(item => fmt_comma(item.popchng));
popchng_pct_arr = popchng_flat.map(item => item.pctchng);
popchng_pct_fmt = popchng_flat.map(item => fmt_pct1(item.pctchng));


if(unit == 'percent') {
var popchng_trace = { 
               x: popchng_pct_arr,
               y : popchng_cat_arr,
			   type : 'bar',
			   orientation : 'h',
			   text: popchng_pct_fmt.map(String),
			  textposition: 'auto',
			  hoverinfo: 'none',
			  marker: {
				opacity: 0.9,
				line: {
				  color: 'blue',
				  width: 1.5
				}
			  }
			};
var axis_spec = {
	title : 'Percent Change',
	showgrid: true,
	zeroline: true,
	showline: true,
	mirror: 'ticks',
	gridcolor: '#e5e4e2',
	gridwidth: 1,
	linecolor: 'black',
	linewidth: 2,
	tickformat: ',.0%'
};
} else {
var popchng_trace = { 
               x: popchng_pop_arr,
               y : popchng_cat_arr,
			   type : 'bar',
			   orientation : 'h',
			   text: popchng_pop_fmt.map(String),
			  textposition : 'auto',
			  hoverinfo: 'none',
			  marker: {
				opacity: 0.9,
				line: {
				  color: 'blue',
				  width: 1.5
				}
			  }
			};

var axis_spec = {
	title : 'Change',
	showgrid: true,
	zeroline: true,
	showline: true,
	mirror: 'ticks',
	gridcolor: '#e5e4e2',
	gridwidth: 1,
	linecolor: 'black',
	linewidth: 2,
	tickformat: ','
};	
};		
var popchng_tr = [popchng_trace];

var popchng_layout = {
		title: "Projected Population Change by Age Group, " + yrvalue + " to "+ yr10 + ", " + ctyName,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: axis_spec, 
		  yaxis: { 
            autorange : 'reversed',
			automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#e5e4e2',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  font : { color : 'black' },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 Plotly.newPlot(POPCHNG, popchng_tr, popchng_layout,config);


//Popchng
 var popchng_names = {
	        fips : "FIPS",
			loc : "Location",
	        age_cat : "Age Group",
			p0 : "Population " + yrvalue,
			p1 : "Population " + yr10,
			popchng : "Population Change",
            pctchng : "Percent Change"
			};


 var popchng_shift = [];
 popchng.forEach( d => {
	 popchng_shift.push({
		               fips : fips,
					   loc : ctyName,
		               age_cat : d.age_cat,
	                   p0 : fmt_comma(d.p0),
					   p1 : fmt_comma(d.p1),
					   popchng : d.popchng < 0 ? ("-" + fmt_comma(Math.abs(d.popchng))) : fmt_comma(d.popchng),
	                   pctchng: d.pctchng < 0 ? ("-" + fmt_pct1(Math.abs(d.pctchng))) : fmt_pct1(d.pctchng)
	 })
 });
 
 var popchng_out = changeKeyObjects(popchng_shift,popchng_names);
 

//

var popchng_csv = document.getElementById('popchng_csv');
var popchng_png = document.getElementById('popchng_png');
popchng_csv.onclick = function() {exportToCsv(ctyName, 'popchng', popchng_out,0)}; 
popchng_png.onclick = function() {exportToPng(ctyName, 'popchng', POPCHNG,0)};

}; 
// popchngPlot


function netmigPlot(inData, app, plotdiv, fips, ctyName) {
//netmigPlot Component Functions for Demograpic Dashboard: Net Migration by Age 

	const fmt_date = d3.timeFormat("%B %d, %Y");
	const fmt_pct = d3.format(".0%");
	const fmt_pct1 = d3.format(".1%");
	const fmt_comma = d3.format(",");
	var config = {responsive: true,
              displayModeBar: false};


//Clearing out divs
var NETMIG = document.getElementById(plotdiv);

NETMIG.innerHTML = "";

//Net Migration 
var NetMigAge =[];

netmig_flat = inData.sort(function(a, b){ return d3.ascending(parseInt(a['age']), parseInt(b['age'])); });
NetMigAge = netmig_flat.map(item => item.age.replace("_", " to "));
NetMig1020 = netmig_flat.map(item => parseInt(item.netmigration));

//plotting

var NetMig1020_bar = { 
               x: NetMigAge,
               y : NetMig1020,
			   name : '2010 to 2020',
			   type : 'bar'
			};
var NetMig_chart = [NetMig1020_bar];

var NetMig_layout = {
		title: "Net Migration by Age -- Net Migrants 2010-2020 " + ctyName,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Net Migration',
			  automargin : true,
			showgrid: true,
			showline: true,
			zeroline : true,
			zerolinewidth: 4,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};

Plotly.newPlot(NETMIG, NetMig_chart, NetMig_layout,config);

//Output
var mig_csv = document.getElementById('mig_csv');
var mig_png = document.getElementById('mig_png');
mig_csv.onclick = function() {exportToCsv(ctyName, 'netmig', netmig_flat,0)}; 
mig_png.onclick = function() {exportToPng(ctyName, 'netmig', NETMIG,0)};

}; 
// netmigPlot

function cocPlot(inData,app, level, plotdiv, bkmark,yrvalue,fips,ctyName) {
//cocPlot Component Functions for Demograpic Dashboard Components of Change Plot Bith Dashboard and Profile
	const fmt_date = d3.timeFormat("%B %d, %Y");
	const fmt_pct = d3.format(".0%");
	const fmt_pct1 = d3.format(".1%");
	const fmt_comma = d3.format(",");
	var config = {responsive: true,
              displayModeBar: false};

var coc_flat = inData;

//Calculating total population change for coc_flat
for(i = 1; i < coc_flat.length; i++){
    coc_flat[i].popchng = coc_flat[i].totalpopulation - coc_flat[i-1].totalpopulation;
    };

if(app == "profile") {
	  pgSetupPro(level,"chart",plotdiv,bkmark,true,false,fips, ctyName, 0)
	  var COC = document.getElementById('PlotDiv4');
} else {
	var COC = document.getElementById(plotdiv);
}

COC.innerHTML = "";

//Components of Change
var year_coc_arr =[];
var pop_coc_arr = [];
var birth_coc_arr = [];
var death_coc_arr = [];
var incr_coc_arr = [];
var migr_coc_arr = [];


coc_flat = coc_flat.sort(function(a, b){ return d3.ascending(a['year'], b['year']); });

year_coc_arr = coc_flat.map(item => item.year);
pop_coc_arr = coc_flat.map(item => item.popchng);
birth_coc_arr = coc_flat.map(item => item.births);
death_coc_arr = coc_flat.map(item => item.deaths);
incr_coc_arr = coc_flat.map(item => item.naturalincrease);
migr_coc_arr = coc_flat.map(item => item.netmigration);

var coc_trace1 = { 
               x: year_coc_arr,
               y : pop_coc_arr,
			   name : 'Total Population Change',
			   mode : 'lines+markers',
			    marker: {
                  color: 'black',
				  symbol: 'circle',
                  size: 6
  },
  line: {
    color: 'black',
    width: 1
  }
			};
			
var coc_trace2 = { 
               x: year_coc_arr,
               y : birth_coc_arr,
			   name : 'Births',
			   mode : 'lines+markers',
			    marker: {
                  color: 'blue',
				  symbol: 'square',
                  size: 4
  },
  line: {
    color: 'plue',
	dash: 'dashdot',
    width: 1
  }
 };

var coc_trace3 = { 
               x: year_coc_arr,
               y : death_coc_arr,
			   name : 'Deaths',
			   mode : 'lines+markers',
			    marker: {
                  color: 'purple',
				  symbol: 'diamond',
                  size: 4
  },
  line: {
    color: 'purple',
	dash: 'dashdot',
    width: 1
  }
};

var coc_trace4 = { 
               x: year_coc_arr,
               y : migr_coc_arr,
			   name : 'Net Migration',
			   mode : 'lines+markers',
			    marker: {
                  color: 'green',
				  symbol: 'diamond',
                  size: 4
  },
  line: {
    color: 'green',
	dash: 'dashdot',
    width: 1
  }
			};
var coc_data = [coc_trace1, coc_trace2, coc_trace3, coc_trace4]

var coc_layout = {
		title: "Births, Deaths and Net Migration 1985 to " + yrvalue + ", " + ctyName,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			title : 'Population Change',
			automargin : true,
			showgrid: true,
			showline: true,
			zeroline : true,
			zerolinewidth: 4,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(COC, coc_data, coc_layout,config);	

var coc_names = {
	    fips : 'FIPS',
		name : 'Location',
		year : 'Year',
		totalpopulation : 'Total Population',
		births : 'Births',
		deaths : 'Deaths',
		naturalincrease : 'Natural Increase',
		netmigration : 'Net Migration'
}

var coc_shift = [];
coc_flat.forEach( d => {
	coc_shift.push({
		   fips : fips,
		   name : ctyName,
		   year : d.year,
		   totalpopulation : d.totapopulation < 0 ?  ("-" + fmt_comma(Math.abs(d.totalpopulation))) : fmt_comma(d.totalpopulation),
		   births : d.births < 0 ?  ("-" + fmt_comma(Math.abs(d.births))) : fmt_comma(d.births),
		   deaths : d.deaths < 0 ?  ("-" + fmt_comma(Math.abs(d.deaths))) : fmt_comma(d.deaths),
           naturalincrease : d.naturalincrease < 0 ?  ("-" + fmt_comma(Math.abs(d.naturalincrease))) : fmt_comma(d.naturalincrease),
		   netmigration : d.netmigration < 0 ?  ("-" + fmt_comma(Math.abs(d.netmigration))) : fmt_comma(d.netmigration)
	})
});
	
var coc_out = changeKeyObjects(coc_shift,coc_names);

if(app == 'profile') {
	var profileDat4 = document.getElementById('profileDat4');
	var profileImg4 = document.getElementById('profileImg4');
	profileDat4.onclick = function() {exportToCsv(ctyName, 'coc', coc_out,0)};
    profileImg4.onclick = function() {exportToPng(ctyName, 'coc', COC,0)};
} else {
	var coc_csv = document.getElementById('coc_csv');
	var coc_png = document.getElementById('coc_png');
	coc_csv.onclick = function() { exportToCsv(ctyName, 'coc', coc_out,0)}; 
	coc_png.onclick = function() {exportToPng(ctyName, 'coc', COC,0)};
}	
}; 
// cocPlot


function genDEMO(geotype, fips, unit, ctyName, yrvalue){
//genDEMO outputs Plotly charts for the Demographic Dashboard
//genDEMO Creates 3 datasets, one for estimates (pop by year), 
// one for net migration by age for 2000-2010, and
// one for forecasts population by age and year

    const fmt_date = d3.timeFormat("%B %d, %Y");
	const fmt_pct1 = d3.format(".1%");
	const fmt_comma = d3.format(",");
    var endyr = yrvalue + 10;
	var fips_list; 

	if(geotype == "region"){
		var fips_tmp = regionCOL(parseInt(fips));
	    fips_list = fips_tmp[0].fips;
		for(i = 0; i < fips_list.length; i++){
			 fips_list[i] = parseInt(fips_list[i]);
		}
	} else {
	if(fips == "000") {
      fips_list = [1,3,5,7,9,11,13,14,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125];
    } else {
		fips_list = [parseInt(fips)];
	};		
	};
//Estimates and components of change chart

	var yr_list = 1985;
	for(i = 1986; i <= yrvalue; i++){
		yr_list = yr_list + "," + i;
	};
	
	var esturl = "https://gis.dola.colorado.gov/lookups/profile?county=" + fips_list + "&year=" + yr_list + "&vars=totalpopulation,births,deaths,naturalincrease,netmigration";
	
//forecasts and age projections
   var forc_yrs = 2010;
   	for(i = 2011; i <= 2050; i++){
		forc_yrs = forc_yrs + "," + i;
	};
		var forcurl = "https://gis.dola.colorado.gov/lookups/sya?county=" + fips_list + "&year=" + forc_yrs + "&choice=single&group=3"


//Net migration by age -- this is net migration by 5-year data
   var netmigurl = "https://storage.googleapis.com/co-publicdata/Colorado_Age_Migration_By_Decade.csv"


var prom = [d3.json(esturl),d3.json(forcurl),d3.csv(netmigurl)];


Promise.all(prom).then(function(data){

//Rollup for region
if(geotype == "region"){
//Estimates
	var columnsEst = ['totalpopulation', 'births', 'deaths', 'naturalincrease','netmigration'];
 	var est_sum =      d3.rollup(data[0], v => Object.fromEntries(columnsEst.map(col => [col, d3.sum(v, d => +d[col])])), d => d.year);
//Flatten Arrays
	var est_data = [];
	for (let [key, value] of est_sum) {
	  est_data.push({'type' : 'region', 'fips' : parseInt(fips), 'name' : ctyName, 'year' : key, 'totalpopulation' : value.totalpopulation, 
	   'births' : value.births, 'deaths' : value.deaths, 'naturalincrease' : value.naturalincrease, 
	   'netmigration' : value.netmigration});
		}

//Forecast
	var columnsFor = ['totalpopulation'];
	var forecast_sum = d3.rollup(data[1], v => Object.fromEntries(columnsFor.map(col => [col, d3.sum(v, d => +d[col])])), d => d.year, d => d.age);

	var forecast_data = [];
	for (let [key1, value] of forecast_sum) {
		for (let[key2, value2] of value) {
		   forecast_data.push({'type' : 'region', 'fips' : parseInt(fips), 'name' : ctyName, 'year' : key1, 'age' : key2, 'totalpopulation' : value2.totalpopulation});
		}
	};

//NetMig SEE NOTE ABOVE

var columnsNet = ['netmigration'];
var netMig_filt = data[2].filter(d => (fips_list.includes(parseInt(d.countyfips))) && (parseInt(d.year) == 2010));
var netmig_sum  =  d3.rollup(netMig_filt, v => Object.fromEntries(columnsNet.map(col => [col, d3.sum(v, d => +d[col])])), d => d.age);

var netmig_data = [];
   for (let [key, value] of netmig_sum) {
	  netmig_data.push({'type' : 'region', 'fips' : parseInt(fips), 'name' : ctyName, 'age' : key,   'netmigration' : value.netmigration});
		}

} else {
	if(fips == "000") { //Colorado as a whole
	//estimate
	var columnsEst = ['totalpopulation', 'births', 'deaths', 'naturalincrease','netmigration'];
 	var est_sum =      d3.rollup(data[0], v => Object.fromEntries(columnsEst.map(col => [col, d3.sum(v, d => +d[col])])), d => d.year);
	var est_data = [];
	for (let [key, value] of est_sum) {
	  est_data.push({'type' : 'state', 'fips' : 0, 'name' : 'Colorado', 'year' : key, 'totalpopulation' : value.totalpopulation, 
	   'births' : value.births, 'deaths' : value.deaths, 'naturalincrease' : value.naturalincrease, 
	   'netmigration' : value.netmigration});
		};

	//Forecast
	var columnsFor = ['totalpopulation'];
	var forecast_sum = d3.rollup(data[1], v => Object.fromEntries(columnsFor.map(col => [col, d3.sum(v, d => +d[col])])), d => d.year, d => d.age);

	var forecast_data = [];
	for (let [key1, value] of forecast_sum) {
		for (let[key2, value2] of value) {
		   forecast_data.push({'type' : 'state', 'fips' : 0, 'name' : 'Colorado', 'year' : key1, 'age' : key2, 'totalpopulation' : value2.totalpopulation});
		}
	};	
	//NetMig
	var netmig_data = data[2].filter(d => (parseInt(fips) == parseInt(d.countyfips)) && (parseInt(d.year) == 2010))

	} else {
	var est_data = [];
	for(i = 0; i < data[0].length; i++){
		  est_data.push({'type' : 'county', 'fips' : data[0][i].countyfips, 'name' : countyName(data[0][i].countyfips),
		  'year' : data[0][i].year, 'totalpopulation' : data[0][i].totalpopulation, 
		   'births' : data[0][i].births, 'deaths' : data[0][i].deaths, 'naturalincrease' : data[0][i].naturalincrease, 
		   'netmigration' : data[0][i].netmigration});
		}
	
	var forecast_data = [];
	for (i = 0; i < data[1].length; i++) {
		   forecast_data.push({'type' : 'county', 'fips' : data[1][i].countyfips, 'name' : countyName(data[1][i].countyfips), 'year' : data[1][i].year,
        		   'age' : data[1][i].age, 'totalpopulation' : data[1][i].totalpopulation});
		}

	var netmig_data = data[2].filter(d => (parseInt(fips) == parseInt(d.countyfips)) && (parseInt(d.year) == 2010))
};
}; 

//Plotting 

	estPlot(est_data, "dashboard", "County",  "est_output", "", yrvalue, fips, ctyName);
	forecastPlot(forecast_data, "dashboard", "County", "forec_output", "", yrvalue, fips, ctyName);
	cocPlot(est_data, "dashboard","County", "coc_output", "", yrvalue, fips, ctyName);
	netmigPlot(netmig_data, "dashboard","mig_output", fips, ctyName);
    agePlot(forecast_data,"dashboard", "ageest_output", yrvalue, fips, ctyName);
    popchngPlot(forecast_data,"dashboard", unit, "popchng_output", yrvalue, fips, ctyName);
 
}); //end of promise
} 
// genDEMO

//cat Race by Age Dashboard Functions

function genRACEVIS(geotype, fips,ctyName, yrvalue) {
//genRACEVIS Generates the Race/Ethncity by Age Dashboard
	const fmt_comma = d3.format(",");
    const fmt_date = d3.timeFormat("%B %d, %Y");
	//Verifying if region is input
	if(RegionNum(ctyName) == 0) {
		geotype = 'county'
	}

//Specify fips_list
 var fips_list; 
	
	if(geotype == "region"){
		var fips_tmp = regionCOL(parseInt(fips));
	    fips_list = fips_tmp[0].fips;
		for(i = 0; i < fips_list.length; i++){
			 fips_list[i] = parseInt(fips_list[i]);
		}
	} else {
	if(fips == "000") {
      fips_list = [1,3,5,7,9,11,13,14,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125];
    } else {
		fips_list = [parseInt(fips)];
	};		
	};
   
//extract year value 
 
var age_list = "0";
for(i = 1; i<= 100; i++){
   age_list = age_list + "," + i;
  };
  
 
//estimates urls  
urlstr_hispest = "https://gis.dola.colorado.gov/lookups/county_sya_race_estimates_current?age="+ age_list + "&county="+ fips_list +"&year="+ yrvalue +"&race=1,2,3,4,5,6&ethnicity=1&sex=b";
urlstr_nonhispest = "https://gis.dola.colorado.gov/lookups/county_sya_race_estimates_current?age="+ age_list + "&county="+ fips_list +"&year="+ yrvalue +"&race=1,2,3,4,5,6&ethnicity=2&sex=b";

//Promise Structure
var prom = [d3.json(urlstr_hispest),d3.json(urlstr_nonhispest)];

Promise.all(prom).then(function(data){
	var hisp_est = [];
    var nonhisp_est = [];
	
if(fips == "000") {	
	data[0].forEach(obj => {
		 hisp_est.push({'fips' : 0,'year' : obj.year, 'age' : parseInt(obj.age), 'sex' : obj.sex, 'population' : parseInt(obj.count)}); 
	    });
    data[1].forEach(obj => {
		nonhisp_est.push({'fips' : 0, 'year' : obj.year, 'age' : parseInt(obj.age),'sex' : obj.sex, 'race' : obj.race, 'population' : parseInt(obj.count)}); 
	    });
} else {
	data[0].forEach(obj => {
		 hisp_est.push({'fips' : obj.county_fips,'year' : obj.year, 'age' : parseInt(obj.age), 'sex' : obj.sex, 'population' : parseInt(obj.count)}); 
	    });
    data[1].forEach(obj => {
		nonhisp_est.push({'fips' : obj.county_fips, 'year' : obj.year, 'age' : parseInt(obj.age),'sex' : obj.sex, 'race' : obj.race, 'population' : parseInt(obj.count)}); 
	    });
};

//Rolling up the hispanic and non-hispanic datasets
var hisp_total = d3.rollup(hisp_est, v => d3.sum(v, d => d.population), d => d.fips, d => d.age);
var nonhisp_total = d3.rollup(nonhisp_est, v => d3.sum(v, d => d.population), d => d.fips,  d => d.age, d=> d.race);
	
//Flattening the datasets
var hisp_flat = [];
for (let [key, value] of hisp_total) {
for (let[key2, value2] of value) {
        hisp_flat.push({'fips' : key, 'name' : countyName(key), 'age' : key2, 'race_eth' : 'Hispanic',  'population' : value2});
    }
}
var nonhisp_flat = [];
for (let [key, value] of nonhisp_total) {
for (let[key2, value2] of value) {
for (let[key3, value3] of value2) { 
   nonhisp_flat.push({'fips' : key, 'name' : countyName(key), 'age' : key2, 'race_eth' : key3 + ' NH', 'population' : value3});
}
}
}

var race_flat = hisp_flat.concat(nonhisp_flat).sort(function(a, b){ return d3.ascending(a['race_eth'], b['race_eth']); })
                .sort(function(a, b){ return d3.ascending(a['age'], b['age']); })
				.sort(function(a, b){ return d3.ascending(a['fips'], b['fips']); });


if(geotype == 'region') {
	var race_reg = d3.rollup(race_flat, v => d3.sum(v, d => d.population),  d => d.age, d=> d.race_eth);
	var race_flat = [];
	for (let [key, value] of race_reg) {
	for (let[key2, value2] of value) {
        race_flat.push({'fips' : -101, 'name' : ctyName, 'age' : key, 'race_eth' : key2,  'population' : value2});
    }
    }

	var race_flat = race_flat.sort(function(a, b){ return d3.ascending(a['race_eth'], b['race_eth']); })
                .sort(function(a, b){ return d3.ascending(a['age'], b['age']); })
				.sort(function(a, b){ return d3.ascending(a['fips'], b['fips']); });
};

//Plotting 
var config = {responsive: true,
              displayModeBar: false};
//Clearing out divs 
var LINE = document.getElementById("line_output");
var WHITE = document.getElementById("white_output");
var HISPANIC = document.getElementById("hisp_output");
var BLACK = document.getElementById("black_output");
var ASIAN = document.getElementById("asian_output");
var AMIND = document.getElementById("amind_output");
var NHPI = document.getElementById("nhpi_output");
var MULTI = document.getElementById("multi_output");

LINE.innerHTML = "";
WHITE.innerHTML = "";
HISPANIC.innerHTML = "";
BLACK.innerHTML = "";
ASIAN.innerHTML = "";
AMIND.innerHTML = "";
NHPI.innerHTML = "";
MULTI.innerHTML = "";

//line chart add traces for line chart

var age_line_arr_w = [];
var pop_line_arr_w = [];
var age_line_arr_h = [];
var pop_line_arr_h = [];
var age_line_arr_b = [];
var pop_line_arr_b = [];
var age_line_arr_as = [];
var pop_line_arr_as = [];
var age_line_arr_ai = [];
var pop_line_arr_ai = [];
var age_line_arr_nh = [];
var pop_line_arr_nh = [];
var age_line_arr_mu = [];
var pop_line_arr_mu = [];

for(i = 0; i < race_flat.length; i++){
	if(race_flat[i].race_eth == "White alone NH" && race_flat[i].age < 85){
		age_line_arr_w.push(race_flat[i].age);
		pop_line_arr_w.push(race_flat[i].population);
	};
	if(race_flat[i].race_eth == "Hispanic" && race_flat[i].age < 85){
		age_line_arr_h.push(race_flat[i].age);
		pop_line_arr_h.push(race_flat[i].population);
	};
	if(race_flat[i].race_eth == "Black or African American alone NH" && race_flat[i].age < 85){
		age_line_arr_b.push(race_flat[i].age);
		pop_line_arr_b.push(race_flat[i].population);
	};
	if(race_flat[i].race_eth == "Asian alone NH" && race_flat[i].age < 85){
		age_line_arr_as.push(race_flat[i].age);
		pop_line_arr_as.push(race_flat[i].population);
	};
	if(race_flat[i].race_eth == "Native Hawaiian or Other Pacific Islander alone NH" && race_flat[i].age < 85){
		age_line_arr_nh.push(race_flat[i].age);
		pop_line_arr_nh.push(race_flat[i].population);
	};
	if(race_flat[i].race_eth == "American Indian and Alaska Native alone NH" && race_flat[i].age < 85){
		age_line_arr_ai.push(race_flat[i].age);
		pop_line_arr_ai.push(race_flat[i].population);
	};
	if(race_flat[i].race_eth == "Two or more NH" && race_flat[i].age < 85){
		age_line_arr_mu.push(race_flat[i].age);
		pop_line_arr_mu.push(race_flat[i].population);
	};
};

var white_line = { 
               x: age_line_arr_w,
               y : pop_line_arr_w,
			   name : 'White, NH',
			   mode : 'lines', 
			   line : {
					color: 'blue',
					width : 3
				}
			};

var hisp_line = { 
               x: age_line_arr_h,
               y : pop_line_arr_h,
			   name : 'Hispanic',
			   mode : 'lines', 
			   line : {
					color: 'orange',
					width : 3
				}
			};

var black_line = { 
               x: age_line_arr_b,
               y : pop_line_arr_b,
			   name : 'Black or African American, NH',
			   mode : 'lines', 
			   line : {
					color: 'green',
					width : 3
				}
			};

var asian_line = { 
               x: age_line_arr_as,
               y : pop_line_arr_as,
			   name : 'Asian, NH',
			   mode : 'lines', 
			   line : {
					color: 'red',
					width : 3
				}
			};
			
var nhpi_line = { 
               x: age_line_arr_nh,
               y : pop_line_arr_nh,
			   name : 'Native Hawaiian or Other Pacific Islander, NH',
			   mode : 'lines', 
			   line : {
					color: 'brown',
					width : 3
				}
			};
var amind_line = { 
               x: age_line_arr_ai,
               y : pop_line_arr_ai,
			   name : 'American Indian and Alaska Native, NH',
			   mode : 'lines', 
    		   line : {
					color: 'purple',
					width : 3
				}
			};

var multi_line = { 
               x: age_line_arr_mu,
               y : pop_line_arr_mu,
			   name : 'Two or More Races, NH',
			   mode : 'lines', 
    		   line : {
					color: 'grey',
					width : 3
				}
			};
//Traces for Barcharts 
var white_bar = { 
               x: age_line_arr_w,
               y : pop_line_arr_w,
			   type : 'bar',
			   marker : { color: 'blue' }
			};

var hisp_bar = { 
               x: age_line_arr_h,
               y : pop_line_arr_h,
			   type : 'bar',
			   marker : { color : 'orange'}
			};

var black_bar = { 
               x: age_line_arr_b,
               y : pop_line_arr_b,
			   type : 'bar',
			   marker : { color: 'green' }
			};

var asian_bar = { 
               x: age_line_arr_as,
               y : pop_line_arr_as,
			   type : 'bar',
			   marker : { color : 'red' }
			};

var nhpi_bar = { 
               x: age_line_arr_nh,
               y : pop_line_arr_nh,
			   type : 'bar',
			   marker : { color : 'brown' }
			};

var amind_bar = { 
               x: age_line_arr_ai,
               y : pop_line_arr_ai,
			   type : 'bar',
			   marker : { color : 'purple' }
			};
			
var multi_bar = { 
               x: age_line_arr_mu,
               y : pop_line_arr_mu,
			   type : 'bar',
			   marker : { color : 'grey' }
			};
//aDD FOR NEW RACES
var line_data = [white_line, hisp_line, black_line, asian_line, nhpi_line, amind_line, multi_line];
var white_trace = [white_bar];
var hisp_trace = [hisp_bar];
var black_trace = [black_bar];
var asian_trace = [asian_bar];
var nhpi_trace = [nhpi_bar];
var amind_trace = [amind_bar];
var multi_trace = [multi_bar];

var line_layout = {
		title: "Single Year of Age by Race/Ethnicity: " + ctyName + ", " + yrvalue,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Total Population',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(LINE, line_data, line_layout,config);

var white_layout = {
		title: "Single Year of Age by Race/Ethnicity: " + ctyName + ", " + yrvalue + " White, NH",
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Total Population',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(WHITE, white_trace, white_layout,config);

var hisp_layout = {
		title: "Single Year of Age by Race/Ethnicity: " + ctyName + ", " + yrvalue + " Hispanic",
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Total Population',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(HISPANIC, hisp_trace, hisp_layout,config);

var black_layout = {
		title: "Single Year of Age by Race/Ethnicity: " + ctyName + ", " + yrvalue + " Black or African American, NH",
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Total Population',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(BLACK, black_trace, black_layout,config);

var asian_layout = {
		title: "Single Year of Age by Race/Ethnicity: " + ctyName + ", " + yrvalue + " Asian, NH",
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Total Population',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(ASIAN, asian_trace, asian_layout,config);

var nhpi_layout = {
		title: "Single Year of Age by Race/Ethnicity: " + ctyName + ", " + yrvalue + " Native Hawaiian or Other Pacific Islander, NH",
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Total Population',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(NHPI, nhpi_trace, nhpi_layout,config);

var amind_layout = {
		title: "Single Year of Age by Race/Ethnicity: " + ctyName + ", " + yrvalue + " American Indian and Alaska Native, NH",
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Total Population',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(AMIND, amind_trace, amind_layout,config);

var multi_layout = {
		title: "Single Year of Age by Race/Ethnicity: " + ctyName + ", " + yrvalue + " Two or More Races, NH",
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Total Population',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
Plotly.newPlot(MULTI, multi_trace, multi_layout,config);

//Download trigger events  ADD NEW RACES HERE...
// Make race_flat wide
var race_wide = restructureRace(race_flat);

//Line Chart
var line_csv = document.getElementById('line_csv');
var line_png = document.getElementById('line_png');
line_csv.onclick = function() {
	  exportToCsv(ctyName, 'line', race_wide, yrvalue);
     }; 
line_png.onclick = function() {
	   exportToPng(ctyName, 'line', LINE, yrvalue);
     };
	 
//WhiteChart
var white_csv = document.getElementById('white_csv');
var white_png = document.getElementById('white_png');
white_csv.onclick = function() {
	  exportToCsv(ctyName, 'white', race_flat.filter(function(d) {return d.race_eth == "White NH";}), yrvalue);
     }; 
white_png.onclick = function() {
	   exportToPng(ctyName, 'white', WHITE, yrvalue);
     };
	 
//Hispanic Chart
var hisp_csv = document.getElementById('hisp_csv');
var hisp_png = document.getElementById('hisp_png');
hisp_csv.onclick = function() {
	  exportToCsv(ctyName, 'hisp', race_flat.filter(function(d) {return d.race_eth == "Hispanic";}), yrvalue);
     }; 
hisp_png.onclick = function() {
	   exportToPng(ctyName, 'hisp', HISPANIC,yrvalue);
     };
	 
//Black Chart
var black_csv = document.getElementById('black_csv');
var black_png = document.getElementById('black_png');
black_csv.onclick = function() {
	  exportToCsv(ctyName, 'black', race_flat.filter(function(d) {return d.race_eth == "Black NH";}), yrvalue);
     }; 
black_png.onclick = function() {
	   exportToPng(ctyName, 'black', BLACK,yrvalue);
     };
	 

//asian Chart
var asian_csv = document.getElementById('asian_csv');
var asian_png = document.getElementById('asian_png');
asian_csv.onclick = function() {
	  exportToCsv(ctyName, 'asian', race_flat.filter(function(d) {return d.race_eth == "Asian NH";}), yrvalue);
     }; 
asian_png.onclick = function() {
	   exportToPng(ctyName, 'asian', ASIAN,yrvalue);
     };
	 
//nhpi Chart
var nhpi_csv = document.getElementById('nhpi_csv');
var nhpi_png = document.getElementById('nhpi_png');
nhpi_csv.onclick = function() {
	  exportToCsv(ctyName, 'nhpi', race_flat.filter(function(d) {return d.race_eth == "Native Hawaiian or Other Pacific Islander NH";}), yrvalue);
     }; 
nhpi_png.onclick = function() {
	   exportToPng(ctyName, 'nhpi', NHPI,yrvalue);
     };
	 
//Amind Chart
var amind_csv = document.getElementById('amind_csv');
var amind_png = document.getElementById('amind_png');
amind_csv.onclick = function() {
	  exportToCsv(ctyName, 'amind', race_flat.filter(function(d) {return d.race_eth == "American Indian NH";}), yrvalue);
     }; 
amind_png.onclick = function() {
	   exportToPng(ctyName, 'amind', AMIND,yrvalue);
     };
	 
//multi Chart
var multi_csv = document.getElementById('multi_csv');
var multi_png = document.getElementById('multi_png');
multi_csv.onclick = function() {
	  exportToCsv(ctyName, 'multi', race_flat.filter(function(d) {return d.race_eth == "Two or More Races NH";}), yrvalue);
     }; 
multi_png.onclick = function() {
	   exportToPng(ctyName, 'multi', MULTI,yrvalue);
     };
}); //End of Promise
}; 
//end of genRaceVis

//cat Net Migration by Age Dashboard

function uniquetwo(arr, keyProps) {
//uniqurtwo generates a uniqe array based on two keys, from stack overflow https://stackoverflow.com/questions/38613654/javascript-find-unique-objects-in-array-based-on-multiple-properties
 const kvArray = arr.map(entry => {
  const key = keyProps.map(k => entry[k]).join('|');
  return [key, entry];
 });
 const map = new Map(kvArray);
 return Array.from(map.values());
}
// uniquetwo

function genNETMIGCOMP(geotype, fips_Arr, yrvalue, chart) {
//genNETMIGCOMP generates the Net Migration by Age Comparison charts 

const fmt_date = d3.timeFormat("%B %d, %Y");
var yr_arr = yrvalue;

//Reading Raw data
var data_csv = "https://storage.googleapis.com/co-publicdata/Colorado_Age_Migration_By_Decade.csv";
d3.csv(data_csv).then(function(data){

   if(geotype == "region"){
	    var reg_cty = []
		var selgeo = [];
		var selgeo_fin = [];
		fips_Arr.forEach(d => {
			var tmpcty = regionCOL(+d);
		    selgeo.push(tmpcty[0].fips)
			tmpcty[0].fips.forEach(c => {
				reg_cty.push({regval : +d, countyfips : parseInt(c).toString()})
			})
		})
		

		for(a = 0; a < selgeo.length; a++){
		for(i = 0; i < selgeo[a].length; i++) {
			selgeo_fin.push(parseInt(selgeo[a][i]).toString())
		}
		}

		
		var datafilt = data.filter(d=> selgeo_fin.includes(d.countyfips) && yrvalue.includes(d.year));

 var reg_data = joinFUNCT(reg_cty,datafilt,"countyfips","countyfips",function(dat,col){
		return{
			regionNum : col.regval,
			countyfips : col.countyfips,
			year : dat.year,
			age: dat.age,
			population : dat.population,
			netmigration : dat.netmigration
		};
	});
	
		var tmp_data = []
		var columnsToSum = ['population', 'netmigration']
		var binroll =  d3.rollup(reg_data, v => Object.fromEntries(columnsToSum.map(col => [col, d3.sum(v, d => +d[col])])), d => d.regionNum, d => d.year, d => d.age);
		for (let [key, value] of binroll) {
		for (let [key2, value2] of value) {
		for (let [key3, value3] of value2) {
		   tmp_data.push({'countyfips' : key,
			            'county' : regionName(key), 
						'year' : key2,
						'age' : key3,
						'population' : value3.population, 
						'netmigration' : value3.netmigration, 
						'migrationrate' : ((value3.netmigration/value3.population) * 100).toFixed(3) 
						});
		};
		}
		}
		var datafilt = tmp_data;
	} else {
		var fipsNum = []
		fips_Arr.forEach(d => fipsNum.push(parseInt(d).toString()))
		var datafilt = data.filter(d => fipsNum.includes(d.countyfips) && yrvalue.includes(d.year));
		 for(i = 0; i < datafilt.length; i++){
		   if(datafilt[i].county == "State Total"){
			   datafilt[i].county = "Colorado";
		   } else {
			   datafilt[i].county = datafilt[i].county + " County"
		   }
	   }
	}

var datasort = datafilt.sort(function(a, b){ return d3.ascending(a['year'], b['year']); })
	                       .sort(function(a, b){ return d3.ascending(a['countyfips'], b['countyfips']); });

//Linetypes for line Charts
var patternArr = ["","/","-","+"]
var lineArr = ['solid','dash','dashdot',"dot"]
var colorArr = ["blue","orange","green","gray"]

//Chart Title
 if((yr_arr.length == 1) && (fips_Arr.length == 1)){
	 var NetMigTitle = "Net Migration by Age -- Net Migrants " + datafilt[0].county + " " + yr_arr[0] + " to " + (parseInt(yr_arr[0]) + 10).toString();
	 var NetMigRateTitle = "Net Migration by Age -- Net Migration Rate<br>" + datafilt[0].county + " " +yr_arr[0] + " to " + (parseInt(yr_arr[0]) + 10).toString()+ "<br>per 100 Population";
 } 
if((yr_arr.length > 1) && (fips_Arr.length == 1)){
	 var NetMigTitle = "Net Migration by Age -- Net Migrants " + datafilt[0].county;
	 var NetMigRateTitle = "Net Migration by Age -- Net Migration Rate<br>" + datafilt[0].county +"<br>per 100 Population";
 } 
if((yr_arr.length == 1) && (fips_Arr.length > 1)){
	 var NetMigTitle = "Net Migration by Age -- Net Migrants " + " " + yr_arr[0] + " to " + (parseInt(yr_arr[0]) + 10).toString();
	 var NetMigRateTitle = "Net Migration by Age -- Net Migration Rate<br>" + yr_arr[0] + " to " + (parseInt(yr_arr[0]) + 10).toString()+ "<br>per 100 Population";
}
if((yr_arr.length > 1) && (fips_Arr.length > 1)){
	 var NetMigTitle = "Net Migration by Age -- Net Migrants ";
	 var NetMigRateTitle = "Net Migration by Age -- Net Migration Rate<br>per 100 Population";
}
 
//Chart Objects 
  var NetMig_trace = [];
  var Rate_trace = [];


for(a = 0; a < fips_Arr.length; a++){
  for(i = 0; i < yr_arr.length; i++){
	  var yr_filt = datafilt.filter(function(d) {return (parseInt(d.countyfips) == parseInt(fips_Arr[a])) && (d.year == yr_arr[i])});
	  var yr_title = parseInt(yr_arr[i]) + " to " + (parseInt(yr_arr[i]) + 10);
      var nameVal = yr_filt[0].county

	  var age_arr = []
	  var netmig = [];
	  var migrate = [];
	  for(j = 0; j < yr_filt.length; j++){
		age_arr.push(yr_filt[j].age.replace("_", " to "))
		netmig.push(+yr_filt[j].netmigration);
		migrate.push(+yr_filt[j].migrationrate);
	  }
	  

if(chart == "bar"){
	  var ind_traceN = {
               x: age_arr,
               y : netmig,
			   name : yr_title,
			   name: nameVal + "<br>  " + yr_title,
			   type : 'bar', 
			   marker :{
				color : colorArr[a],
				pattern: {shape: patternArr[i]},
 			    line: { color : 'black', width : 1 }
				}
			};
      NetMig_trace.push(ind_traceN)

	var ind_traceRT = {
               x: age_arr,
               y : migrate,
			   name: nameVal + "<br>  " + yr_title,
			   type : 'bar', 
			   marker :{
				color : colorArr[a],
				pattern: {shape: patternArr[i]},
 			    line: { color : 'black', width : 1 }
				}
			};
      Rate_trace.push(ind_traceRT)
  } // Bar
  
if(chart == "line"){
	  var ind_traceN = {
               x: age_arr,
               y : netmig,
			   name : yr_title,
			   mode: 'lines+markers',
			   name: nameVal + "<br>  " + yr_title,
			   line: {
                 dash: lineArr[i],
                 width: 3,
				 color : colorArr[a],
				 shape: 'spline'
				}
			};
      NetMig_trace.push(ind_traceN)

	var ind_traceRT = {
               x: age_arr,
               y : migrate,
			   name: nameVal + "<br>  " + yr_title,
			   mode : 'lines+markers',
			   line: {
                 dash: lineArr[i],
                 width: 3,
				 color : colorArr[a],
				 shape: 'spline'
				}
			};
      Rate_trace.push(ind_traceRT)
  } // Bar

} //Yr Loop
} //geo loop

var NetMig_layout = {
		title: NetMigTitle,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Net Migration',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
		shapes: [{
			type: 'line',
			x0: 0,
			y0: 0,
			x1: 15,
			y1: 0,
			line: {
				color: 'black',
				width: 4,
				dash: 'solid'
		}}],
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
 
 var Rate_layout = {
		title: NetMigRateTitle,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Age',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Net Migration Rate (per 100 Population)',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat:  '.2f'
		  },
		shapes: [{
			type: 'line',
			x0: 0,
			y0: 0,
			x1: 15,
			y1: 0,
			line: {
				color: 'black',
				width: 4,
				dash: 'solid'
		}}],
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
		
var config = {responsive: true,
              displayModeBar: false};
//Clearing out divs
var NETMIG = document.getElementById("netmign_output");
var NETMIGRATE = document.getElementById("netmigrrate_output");

NETMIG.innerHTML = "";
NETMIGRATE.innerHTML = "";

Plotly.newPlot(NETMIG, NetMig_trace, NetMig_layout,config);
Plotly.newPlot(NETMIGRATE, Rate_trace, Rate_layout,config);


//Button Events
//Net Migration Chart

if(fips_Arr. length == 1){
	var outName = nameVal;
} else {
	if(geotype == "Region") {
	var outName = "Multiple Regions"
	} else {
	var outName = "Multiple Counties"
	}
}

var netmign_csv = document.getElementById('netmign_csv');
var netmign_png = document.getElementById('netmign_png');
netmign_csv.onclick = function() {
	  exportToCsv(outName, 'netmign', datasort, 0);
     }; 
netmign_png.onclick = function() {
	   exportToPng(outName, 'netmign', NETMIG, 0);
     };
	 
// Net Migration Rate
var netmigrrate_csv = document.getElementById('netmigrrate_csv');
var netmigrrate_png = document.getElementById('netmigrrate_png');
netmigrrate_csv.onclick = function() {
	  exportToCsv(outName, 'netmigrrate', datasort, 0);
     }; 
netmigrrate_png.onclick = function() {
	   exportToPng(outName, 'netmigrrate', NETMIGRATE, 0);
     };
	 

}); //End of d3.csv
} 
// genNETMIGCOMP

//cat Net Migration by Working Age Dsahboard

function genNETMIG1864(fipsVal, ctyName, ageSeries, chartType, yrvalue){
//genNETMIG1864 Historical NetMigration charts, inclusing working age population was net_mig 1864
//Uses data from netmig_1864x Must be updated after Census 2020 is available

const fmt_date = d3.timeFormat("%B %d, %Y");

var data_csv = "../data/netmig_1864x.csv";

	
//Building Chart Title and filename

var titStr = "Net Migration by Year: ";

if(ctyName.length == 1) {
	titStr = titStr + ctyName[0];
   } else { 
	   for (i = 0; i < ctyName.length; i++){
		 if(i == (ctyName.length -1)){
			 titStr = titStr + " and " + ctyName[i];
		 } else {
		if (ctyName.length == 2){
			titStr = titStr +  ctyName[i] + " ";
		} else {
          titStr = titStr +  ctyName[i] + ", ";
		 }
       }
	   }
   }; 
   
//Creating second line of title
if(ageSeries == "ageall"){	   
	 var titStrTot = titStr + "<br>Total Population Counts";
	 var  titStrRate = titStr + "<br>Total Population Rate per 100";
	} else {
		var titStrTot = titStr + "<br>Working Age Population Counts (Age 18-64)";
		var titStrRate = titStr + "<br>Working Age Population Rate per 100 (Age 18-64)";
	};


	
d3.csv(data_csv).then(function(data){
	var tot_trace = [];
	var rate_trace = [];

	if(fipsVal.length == 1) {
		var datafilt = data.filter(function(d,i) {return d.fips == fipsVal[0];});
	} else {
		var datafilt = data.filter(function(d,i) {return fipsVal.indexOf(d.fips)  >= 0;});
	};

		for(i = 0; i < fipsVal.length; i++) {
			var datafilt_multi = datafilt.filter(function(d) {return d.fips == fipsVal[i];});
			var xDataYear = [];
			var yDataTot = [];
			var yDataRate = [];
			for(j = 0; j < datafilt_multi.length; j++) {
			  if(ageSeries == 'age1864'){
				  if(datafilt_multi[j].year >= 1990){
				   xDataYear.push(Number(datafilt_multi[j].year));
				   yDataTot.push(Number(datafilt_multi[j].netmigration_1864));
				   yDataRate.push(Number(datafilt_multi[j].rate_1864));
				  }
				} else {  
				  xDataYear.push(Number(datafilt_multi[j].year));
				  yDataTot.push(Number(datafilt_multi[j].netmigration_total));
				  yDataRate.push(Number(datafilt_multi[j].rate_total));
			};
			};

if(chartType == 'bar'){
		var tot_tmp = { 
					   x: xDataYear,
					   y :  yDataTot,
					   name : ctyName[i],
					   type : 'bar'
					};
		var rate_tmp = { 
					   x: xDataYear,
					   y : yDataRate,
					   name : ctyName[i],
					   type : 'bar'
					};			
	} else {
		var tot_tmp = { 
					   x: xDataYear,
					   y : yDataTot,
					   name : ctyName[i],
					   mode : 'lines+markers'
					};
		var rate_tmp = { 
					   x: xDataYear,
					   y : yDataRate,
					   name : ctyName[i],
					   mode : 'lines+markers'
					};			
};

tot_trace.push(tot_tmp);
rate_trace.push(rate_tmp);
};


	


//Generating Chart	
var config = {responsive: true,
              displayModeBar: false};
			  
	
//Clearing out Divs
var NETMIGTOT = document.getElementById("netmigtot_output");
var NETMIGRATE = document.getElementById("netmigrrate_output");

NETMIGTOT.innerHTML = "";
NETMIGRATE.innerHTML = "";

var tot_layout = {
		title: titStrTot,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Persons',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};


var rate_layout = {
		title: titStrRate,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			  title : 'Rate per 100 Persons',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat:  '.2f'
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
		//Chart Call

Plotly.newPlot(NETMIGTOT, tot_trace, tot_layout, config);
Plotly.newPlot(NETMIGRATE, rate_trace, rate_layout, config);

//Button Events
//Net Migration Chart

var netmigwa_csv = document.getElementById('netmigwa_csv');
var netmigwa_png = document.getElementById('netmigwa_png');
netmigwa_csv.onclick = function() {
	  exportToCsv(ctyName, 'netmigwa', datafilt, 0);
     }; 
netmigwa_png.onclick = function() {
	   exportToPng(ctyName, 'netmigwa', NETMIGTOT, 0);
     };
	 
// Net Migration Rate

var netmigrwa_csv = document.getElementById('netmigrwa_csv');
var netmigrwa_png = document.getElementById('netmigrwa_png');
netmigrwa_csv.onclick = function() {
	  exportToCsv(ctyName, 'netmigrwa', datafilt, 0);
     }; 
netmigrwa_png.onclick = function() {
	   exportToPng(ctyName, 'netmigrwa', NETMIGRATE, 0);
     };
	 
}); //end of d3 csv
}; 
//  genNETMIG1864

//cat Long term components of change dashboard (netmighist.html)

function genCOCHIST(fipsVal,  byrs, eyrs, stats, axis, DIV0, DIV1) {
//genCOCHIST generates long-term COC charts
	
const fmt_date = d3.timeFormat("%B %d, %Y");

//Generating urls
var ctyfips  = parseInt(fipsVal);
var ctyName = countyName(ctyfips);

var yrlist = parseInt(byrs);
for(i = parseInt(byrs)+1; i <= parseInt(eyrs); i++){
	yrlist = yrlist + "," + i;
};




if(fipsVal == "000") {
	 var dataurl = 'https://gis.dola.colorado.gov/lookups/components_region?reg_num=' + ctyfips + '&year=' + yrlist;
} else {
	var dataurl = 'https://gis.dola.colorado.gov/lookups/components?county=' + ctyfips + '&year=' + yrlist;
}


var year_est = [];
var year_forc = [];
var year_bars = [];
var year_tick = []
var birth_est = [];
var birth_forc = [];
var death_est = [];
var death_forc = [];
var mig_est = [];
var mig_forc = [];
var mig_bars = [];
var natincr_est = [];
var natincr_forc = [];
var natincr_bars = []
var out_data = [];

d3.json(dataurl).then(function(data){

	   for(i = 0; i < data.length; i++){
		   out_data.push({ 'FIPS' : fipsVal[0], 'County' : ctyName, 'Year' : data[i].year, 'Births' : Number(data[i].births), 'Deaths' : Number(data[i].deaths), "Natural Increase" : Number(data[i].births) - Number(data[i].deaths),
		                  'Net Migration' : Number(data[i].netmig), 'Data Type' : data[i].datatype});
		   if(data[i].datatype == "Estimate"){
			year_est.push(data[i].year);
		    birth_est.push(Number(data[i].births));
			death_est.push(Number(data[i].deaths));
			mig_est.push(Number(data[i].netmig));
			natincr_est.push(Number(data[i].births) - Number(data[i].deaths));
		   } else {
			year_forc.push(data[i].year);
		    birth_forc.push(Number(data[i].births));
			death_forc.push(Number(data[i].deaths));
			mig_forc.push(Number(data[i].netmig));
			natincr_forc.push(Number(data[i].births) - Number(data[i].deaths));
		   };
		   //Dealing with axis ticks
			switch(axis) {
				case "yr5" :
				   if(i % 5 == 0){
					   year_tick.push(data[i].year)
				   }
				   break;
				case "yr2" : 
				   if(i % 2 == 0){
					   year_tick.push(data[i].year)
				   }
				   break;
				case "yr1" :
					   year_tick.push(data[i].year)
					   break;
			}

		  year_bars.push(data[i].year);
		  mig_bars.push(Number(data[i].netmig));
		  natincr_bars.push(Number(data[i].births) - Number(data[i].deaths));

	   };
	   


var min_yr = Math.min(...year_tick);
var max_yr = Math.max(...year_tick);
var tit1 = "Components of Population Change "
var bar_title =  tit1.concat(min_yr.toString(), " to ", max_yr.toString(), ": ", ctyName) 

//Line Traces
var birth_tmp1 = { 
					   x: year_est,
					   y : birth_est,
					   name : 'Births Estimate',
					   mode : 'lines',
					   line: {
						dash: 'solid',
						color : '#d81b60',
						width: 3
						}
					};

var birth_tmp2 = { 
					   x: year_forc,
					   y : birth_forc,
					   name : 'Births Forecast',
					   mode : 'lines',
					   line: {
						dash: 'dash',
						color : '#d81b60',
						width: 3
						}
					};

var death_tmp1 = { 
					   x: year_est,
					   y : death_est,
					   name : 'Deaths Estimate',
					   mode : 'lines',
					   line: {
						dash: 'solid',
						color : '#1e88e5',
						width: 3
						}
					};

var death_tmp2 = { 
					   x: year_forc,
					   y : death_forc,
					   name : 'Deaths Forecast',
					   mode : 'lines',
					   line: {
						dash: 'dash',
						color : '#1e88e5',
						width: 3
						}
					};

var mig_tmp1 = { 
					   x: year_est,
					   y : mig_est,
					   name : 'Net Migraton Estimate',
					   mode : 'lines',
					   line: {
						dash: 'solid',
						color : '#8B8000',
						width: 3
						}
					};

var mig_tmp2 = { 
					   x: year_forc,
					   y : mig_forc,
					   name : 'Net Migration Forecast',
					   mode : 'lines',
					   line: {
						dash: 'dash',
						color : '#8B8000',
						width: 3
						}
					};

var natincr_tmp1 = { 
					   x: year_est,
					   y : natincr_est,
					   name : 'Natural Increase Estimate',
					   mode : 'lines',
					   line: {
						dash: 'solid',
						color : '#004d40',
						width: 3
						}
					};

var natincr_tmp2 = { 
					   x: year_forc,
					   y : natincr_forc,
					   name : 'Natural Increase Forecast',
					   mode : 'lines',
					   line: {
						dash: 'dash',
						color : '#004d40',
						width: 3
						}
					};
					
//Set up x-axis string


//Creating the line chart trace

var line_trace = []
stats.forEach( d => {
	switch(d){
		case "births":
		line_trace.push(birth_tmp1, birth_tmp2);
		break;
		case "deaths":
		line_trace.push(death_tmp1, death_tmp2);
		break;
		case "netmig":
		line_trace.push(mig_tmp1, mig_tmp2);
		break;
		case "natincr":
		line_trace.push(natincr_tmp1, natincr_tmp2);
		break;
		}
})

//Genearing Bar chart trace

var mig_bar = { 
					   x: year_bars,
					   y : mig_bars,
					   name : 'Net Migration',
					   type : 'bar',
					   marker: {
						color : '#C0504D'
					   }
					};

var natincr_bar = { 
					   x: year_bars,
					   y : natincr_bars,
					   name : 'Natural Increase',
					   type : 'bar',
					   marker : {
						color : '#4F81BD'
					   }
					};
					

var bar_trace = [natincr_bar, mig_bar]

//Generating Chart	
var config = {responsive: true,
              displayModeBar: false};
			  
	
//Clearing out Divs

DIV0.innerHTML = "";
DIV1.innerHTML = "";


var line_layout = {
		title: "Components of Change: " + ctyName,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: '#D3D3D3',
			linewidth: 2,
			tickmode: "array", 
			tickvals : year_tick,
			tickangle: 45,
			tickfont: { size: 10}
		  },
		  yaxis: {
			  title : 'Persons',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
		
		
var bar_layout = {
		title: bar_title,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  barmode : 'relative',
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: true,
			showline: true,
//			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: '#D3D3D3',
			linewidth: 2,
			tickmode: "array", 
			tickvals: year_tick,
			tickangle: 45,
			tickfont: { size: 10}
		  },
		  yaxis: {
			  title : 'Persons',
			  automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			 tickformat: ','
		  },
		  legend : { x : 0.3, y : 1.1, 'orientation' : 'h', font:{size: 10}},
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
		

Plotly.newPlot(DIV0, line_trace, line_layout, config);
Plotly.newPlot(DIV1, bar_trace, bar_layout, config);

//Button Events

var linecoc_csv = document.getElementById('linecoc_csv');
var linecoc_png = document.getElementById('linecoc_png');
linecoc_csv.onclick = function() {
	  exportToCsv(ctyName, 'linecoc', out_data, 0);
     }; 
linecoc_png.onclick = function() {
	   exportToPng(ctyName, 'linecoc', DIV0, 0);
     };
	 

var barcoc_csv = document.getElementById('barcoc_csv');
var barcoc_png = document.getElementById('barcoc_png');
barcoc_csv.onclick = function() {
	  exportToCsv(ctyName, 'barcoc', out_data, 0);
     }; 
barcoc_png.onclick = function() {
	   exportToPng(ctyName, 'barcoc', DIV1, 0);
     };
	 
}); //end of d3 json
}; 
// genCOCHIST

//cat Household Dashboard Functions

function genHOUSEAGE(fipsVal,ctyName, varType, seriesType){
//genHOUSEAGE  Household forecast by age and household type
	const fmt_date = d3.timeFormat("%B %d, %Y");

	var fips_list = parseInt(fipsVal);

   var yr_trace = [2010];

 	var yr_list = 2010;
	for(i = 2011; i <= 2050; i++){
		yr_list = yr_list + "," + i;
		yr_trace.push(Number(i));
	};


var urlstr = "https://gis.dola.colorado.gov/lookups/household?county="+ fips_list + "&year=" + yr_list + "&age=0,1,2,3,4&household=0,1,2,3,4";


var dataplot = [];
var datahh = [];
var dataage = [];
var hh_arr = ["All Households", "One adult with no children", "One adult with children", "More than one adult with no children", "More than one adult with children"]
var age_arr = ["Total", "18-24", "25-44", "45-64","65 and Older"]
d3.json(urlstr).then(function(data){
   data.forEach(function(obj) {
    obj.household = hh_arr[obj.household_type_id];
    obj.age = age_arr[obj.age_group_id];
    dataplot.push({'year' : obj.year, 'household' : obj.household, 'age' : obj.age, 'total_households' : Number(obj.total_households)});
 });
 //Calculating Age by Housing Type percentage

for(i = 2010; i <= 2050;i++){
	var tmp = dataplot.filter(function(d) {return d.year == i;});

	for(j = 0; j < 5;j++){
		var tmp2 = tmp.filter(function(d) {return d.household == hh_arr[j];});
		for(k = 0; k < tmp2.length; k++){
			if(tmp2[k].age == age_arr[0]) {
			  var base_total = tmp2[k].total_households;
			} else {
			  var age_pct = tmp2[k].total_households/base_total;
			  datahh.push({'year' : tmp2[k].year, 'household' : tmp2[k].household, 'age' : tmp2[k].age, 'total_households' : Math.round(tmp2[k].total_households), 'age_pct' : age_pct});
		};
		};
    };
};

 //Calculating Housing Type by Age Group percentage

for(i = 2010; i <= 2050;i++){
	var tmp = dataplot.filter(function(d) {return d.year == i;});

	for(j = 0; j < 5;j++){
		var tmp2 = tmp.filter(function(d) {return d.age == age_arr[j];});
		for(k = 0; k < tmp2.length; k++){
			if(tmp2[k].household == hh_arr[0]) {
			  var base_total = tmp2[k].total_households;
			} else {
			  var hh_pct = tmp2[k].total_households/base_total;
			  dataage.push({'year' : tmp2[k].year, 'household' : tmp2[k].household, 'age' : tmp2[k].age, 'total_households' : Math.round(tmp2[k].total_households), 'hh_pct' : hh_pct});
		};
		};
    };
};

//Preparing charts

var tr_0 = [];  //These are the individual traces
var tr_1 = [];
var tr_2 = [];
var tr_3 = [];
var tr_4 = [];

var ch_0 = [];  //These are the chart traces 5 lines per chart
var ch_1 = [];
var ch_2 = [];
var ch_3 = [];
var ch_4 = [];

if(varType == "hhold") { //Age by HH
for(i = 0; i < hh_arr.length; i++){
	for(j = 1; j < age_arr.length; j++) {
		var tmphh = datahh.filter(function(d) {return d.household == hh_arr[i] && d.age == age_arr[j];});
		var yData = [];
		for(k = 0; k < tmphh.length; k++){
			if(seriesType == "num"){
		       yData.push(tmphh[k].total_households);
			} else {
		       yData.push(tmphh[k].age_pct);
			};	
		};	

	if(j == 0){
   	tr_0 = {x: yr_trace,
			y :  yData,
			name : age_arr[j],
			mode : 'lines+markers'
			};
	};
	if(j == 1){
   	tr_1 = {x: yr_trace,
			y :  yData,
			name : age_arr[j],
			mode : 'lines+markers'
			};
	};
	if(j == 2){
   	tr_2 = {x: yr_trace,
			y :  yData,
			name : age_arr[j],
			mode : 'lines+markers'
			};
	};	
	if(j == 3){
   	tr_3 = {x: yr_trace,
			y :  yData,
			name : age_arr[j],
			mode : 'lines+markers'
			};
	};
	if(j == 4){
   	tr_4 = {x: yr_trace,
			y :  yData,
			name : age_arr[j],
			mode : 'lines+markers'
			};
	};
   	} //J loop
	//Making the compined Traces
	if(i == 0) { ch_0 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
	if(i == 1) { ch_1 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
	if(i == 2) { ch_2 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
	if(i == 3) { ch_3 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
	if(i == 4) { ch_4 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
} //I Loop
} else {  //HH by Age
	for(i = 0; i < age_arr.length; i++){
	for(j = 1; j < hh_arr.length; j++) {
		
		var tmpage = dataage.filter(function(d) {return d.household == hh_arr[j] && d.age == age_arr[i];});
		var yData = [];
		for(k = 0; k < tmpage.length; k++){
			if(seriesType == "num"){
		       yData.push(tmpage[k].total_households);
			} else {
		       yData.push(tmpage[k].hh_pct);
			};	
		};	

	if(j == 0){
   	tr_0 = {x: yr_trace,
			y :  yData,
			name : hh_arr[j],
			mode : 'lines+markers'
			};
	};
	if(j == 1){
   	tr_1 = {x: yr_trace,
			y :  yData,
			name : hh_arr[j],
			mode : 'lines+markers'
			};
	};
	if(j == 2){
   	tr_2 = {x: yr_trace,
			y :  yData,
			name : hh_arr[j],
			mode : 'lines+markers'
			};
	};	
	if(j == 3){
   	tr_3 = {x: yr_trace,
			y :  yData,
			name : hh_arr[j],
			mode : 'lines+markers'
			};
	};
	if(j == 4){
   	tr_4 = {x: yr_trace,
			y :  yData,
			name : hh_arr[j],
			mode : 'lines+markers'
			};
	};
   	} //J loop
	//Making the compined Traces
	if(i == 0) { ch_0 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
	if(i == 1) { ch_1 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
	if(i == 2) { ch_2 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
	if(i == 3) { ch_3 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
	if(i == 4) { ch_4 = [tr_0, tr_1, tr_2, tr_3, tr_4];};
} //I Loop
}; //Trace 	

//Titles
var ch_layout = [];

if(varType == "hhold") {
for(i = 0; i < hh_arr.length; i++){
	var tit_str = "Projected Households by Age and Household Type " + ctyName + " 2010 to 2050<br>Household Type: " + hh_arr[i];
	if(seriesType == "num") {
		tit_str = tit_str + " Number of Households";
		y_title = "Households";
		y_ticks = ',';
    } else {
		tit_str = tit_str + " Percentage of Housing Units";
		y_title = "Percentage";
		y_ticks = ',.0%';
	};
	
//layouts
  var layout = {
		title: tit_str,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			type: 'date'
		  },
		  yaxis: {
			title : y_title,
			automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			tickformat: y_ticks
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
ch_layout.push(layout);
};
} else {
	for(i = 0; i < age_arr.length; i++){
	var tit_str = "Projected Households by Household Type and Age " + ctyName + " 2010 to 2050<br> Age Group: " + age_arr[i];
	if(seriesType == "num") {
		tit_str = tit_str + " Number of Households";
		y_title = "Households";
		y_ticks = ',';
    } else {
		tit_str = tit_str + " Percentage of Households";
		y_title = "Percentage";
		y_ticks = ',.0%';
	};
	
//layouts
  var layout = {
		title: tit_str,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			title : y_title,
			automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			tickformat: y_ticks
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};
ch_layout.push(layout);
};
};

//Generating Chart	
var config = {responsive: true,
              displayModeBar: false};
			  
	
//Clearing out Divs
var CHART0 = document.getElementById("chart0_output");
var CHART1 = document.getElementById("chart1_output");
var CHART2 = document.getElementById("chart2_output");
var CHART3 = document.getElementById("chart3_output");
var CHART4 = document.getElementById("chart4_output");

CHART0.innerHTML = "";
CHART1.innerHTML = "";
CHART2.innerHTML = "";
CHART3.innerHTML = "";
CHART4.innerHTML = "";

Plotly.newPlot(CHART0, ch_0, ch_layout[0], config);
Plotly.newPlot(CHART1, ch_1, ch_layout[1], config);
Plotly.newPlot(CHART2, ch_2, ch_layout[2], config);
Plotly.newPlot(CHART3, ch_3, ch_layout[3], config);
Plotly.newPlot(CHART4, ch_4, ch_layout[4], config);

//Button Events

var chart0_csv = document.getElementById('chart0_csv');
var chart0_png = document.getElementById('chart0_png');
var chart1_csv = document.getElementById('chart1_csv');
var chart1_png = document.getElementById('chart1_png');
var chart2_csv = document.getElementById('chart2_csv');
var chart2_png = document.getElementById('chart2_png');
var chart3_csv = document.getElementById('chart3_csv');
var chart3_png = document.getElementById('chart3_png');
var chart4_csv = document.getElementById('chart4_csv');
var chart4_png = document.getElementById('chart4_png');
if(varType == "hhold") { 
	chart0_csv.onclick = function() { exportToCsv(ctyName, 'hhchart', datahh, 0); }; 
	chart1_csv.onclick = function() { exportToCsv(ctyName, 'hhchart', datahh, 0); }; 
	chart2_csv.onclick = function() { exportToCsv(ctyName, 'hhchart', datahh, 0); }; 
	chart3_csv.onclick = function() { exportToCsv(ctyName, 'hhchart', datahh, 0); }; 
	chart4_csv.onclick = function() { exportToCsv(ctyName, 'hhchart', datahh, 0); }; 
	
	chart0_png.onclick = function() { exportToPng(ctyName, 'hhchart', CHART0, 0); }; 
	chart1_png.onclick = function() { exportToPng(ctyName, 'hhchart', CHART1, 0); }; 
	chart2_png.onclick = function() { exportToPng(ctyName, 'hhchart', CHART2, 0); }; 
	chart3_png.onclick = function() { exportToPng(ctyName, 'hhchart', CHART3, 0); }; 
	chart4_png.onclick = function() { exportToPng(ctyName, 'hhchart', CHART4, 0); }; 
} else {
	chart0_csv.onclick = function() { exportToCsv(ctyName, 'agechart', dataage, 0); }; 
	chart1_csv.onclick = function() { exportToCsv(ctyName, 'agechart', dataage, 0); }; 
	chart2_csv.onclick = function() { exportToCsv(ctyName, 'agechart', dataage, 0); }; 
	chart3_csv.onclick = function() { exportToCsv(ctyName, 'agechart', dataage, 0); }; 
	chart4_csv.onclick = function() { exportToCsv(ctyName, 'agechart', dataage, 0); }; 

	chart0_png.onclick = function() { exportToPng(ctyName, 'agechart', CHART0, 0); }; 
	chart1_png.onclick = function() { exportToPng(ctyName, 'agechart', CHART1, 0); }; 
	chart2_png.onclick = function() { exportToPng(ctyName, 'agechart', CHART2, 0); }; 
	chart3_png.onclick = function() { exportToPng(ctyName, 'agechart', CHART3, 0); }; 
	chart4_png.onclick = function() { exportToPng(ctyName, 'agechart', CHART4, 0); }; 
};	

}); //end of d3 json
}; 
// genHOUSEAGE


function genHOUSEDASH(geotype,fips,plName,yrvalue) {
//genHOUSEDASH Housing Dashboard
	
	const fmt_pct = d3.format(".2%")
	const fmt_comma = d3.format(",");
    const fmt_date = d3.timeFormat("%B %d, %Y");
	
	//Verifying if region is input
	if(geotype =='region'){
	if(RegionNum(plName) == 0) {
		geotype = 'county'
	}
	}
	
	if(geotype == "region"){
		var fips_tmp = regionCOL(parseInt(fips));
	    fips_list = fips_tmp[0].fips;
		for(i = 0; i < fips_list.length; i++){
			 fips_list[i] = parseInt(fips_list[i]);
		}
	} else {
	if(fips == "000") {
      fips_list = [1,3,5,7,9,11,13,14,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125];
    } else {
		fips_list = [parseInt(fips)];
	};		
	};

	//Generating Range of Uears
	//Estimates and components of change chart
	var yr_list = 2010;
	for(i = yr_list; i <= yrvalue; i++){
		yr_list = yr_list + "," + i;
	};
	
	//Generating URL
	if(geotype == "region" || geotype == 'county'){
		huURL = 'https://gis.dola.colorado.gov/lookups/profile?county=' + fips_list + '&year=' + yr_list + '&vars=totalhousingunits,vacanthousingunits';
	} else { //Municipalities
		huURL = 'https://gis.dola.colorado.gov/lookups/munipophousing?year=' + yr_list + '&placefips=' + fips_list + '&stats=totalhousingunits,vacanthousingunits&compressed=no';
	};

//Promise
d3.json(huURL).then(data => {

	var baseData = [];
	data.forEach(obj => {
		baseData.push({
			fips : (geotype == 'municipality') ? obj.placefips : obj.countyfips, 
			year : obj.year,
		    totalhu : parseInt(obj.totalhousingunits),
			vacanthu : parseInt(obj.vacanthousingunits),
			occupiedhu : parseInt(obj.totalhousingunits) - parseInt(obj.vacanthousingunits)
		})
	});

	var out_data = [];
	columnsToSum = ['totalhu', 'occupiedhu', 'vacanthu']
	if(geotype == "region") {
		var regSum = d3.rollup(baseData, v => Object.fromEntries(columnsToSum.map(col => [col, d3.sum(v, d => +d[col])])), d => d.year)
	//Flatten Arrays for output
		for (let [key, value] of regSum) {
		  out_data.push({'fips' : -101, 'name' : regionName(fips), 'year' : key, 
		       'totalhu' : value.totalhu, 
			   'occupiedhu' : value.occupiedhu, 
			   'vacanthu' : value.vacanthu,
			   'pct_occ' : value.occupiedhu/value.totalhu,
			   'pct_vac' : value.vacanthu/value.totalhu});
			 };
	};

	if(geotype == "county") {
		if(fips == '000'){
		var regSum = d3.rollup(baseData, v => Object.fromEntries(columnsToSum.map(col => [col, d3.sum(v, d => +d[col])])), d => d.year)
	//Flatten Arrays for output
		for (let [key, value] of regSum) {
		  out_data.push({'fips' : 0, 'name' : 'Colorado', 'year' : key, 
		       'totalhu' : value.totalhu, 
			   'occupiedhu' : value.occupiedhu, 
			   'vacanthu' : value.vacanthu,
			   'pct_occ' : value.occupiedhu/value.totalhu,
			   'pct_vac' : value.vacanthu/value.totalhu});
			 };
	} else {
		baseData.forEach(obj => {
			out_data.push({ 'fips' : obj.fips, 
			    'name' : countyName(obj.fips), 
				'year' : obj.year, 
		       'totalhu' : obj.totalhu, 
			   'occupiedhu' : obj.occupiedhu, 
			   'vacanthu' : obj.vacanthu,
			   'pct_occ' : obj.occupiedhu/obj.totalhu,  
			   'pct_vac' : obj.vacanthu/obj.totalhu});
		});
	};
	};
	
	if(geotype == 'municipality'){
		baseData.forEach(obj => {
			out_data.push({ 'fips' : obj.fips, 
			'name' : muniName(obj.fips), 
			'year' : obj.year, 
		       'totalhu' : obj.totalhu, 
			   'occupiedhu' : obj.occupiedhu, 
			   'vacanthu' : obj.vacanthu,
			   'pct_occ' : obj.occupiedhu/obj.totalhu,
			   'pct_vac' : obj.vacanthu/obj.totalhu});
	});
	};
	
	var out_sort = out_data.sort(function(a, b){ return d3.ascending(a['year'], b['year']); })
	                       .sort(function(a, b){ return d3.ascending(a['fips'], b['fips']); });
						   

	//calculating year to year difference
	var yearList = [];
	var yty = [];
	var yty_data = [];
	
	//Traces
	var tr_total_hu = [];
	var tr_total_lab = [];
	var tr_occ_hu = [];
	var tr_occ_lab = [];
	var tr_vac_hu = [];
	var tr_occ_pct_lab = [];
	var tr_vac_pct_lab = [];
	var tr_tot_yty = [];
	var tr_occ_yty = [];
	var tr_tot_yty_lab = [];
	var tr_occ_yty_lab = [];
	
	for(j = 0; j < out_sort.length; j++){
		   //Creating Traces
            yearList.push(out_sort[j].year);		   
			tr_total_hu.push(out_sort[j].totalhu);
			tr_total_lab.push('Total Housing Units, ' + out_sort[j].year + ': ' + fmt_comma(out_sort[j].totalhu));
			tr_occ_hu.push(out_sort[j].occupiedhu);
			tr_occ_lab.push('Occupied Housing Units, ' + out_sort[j].year + ': ' + fmt_comma(out_sort[j].occupiedhu));
			tr_vac_hu.push(out_sort[j].vacanthu);
			tr_occ_pct_lab.push('Occupancy Rate, ' + out_sort[j].year + ': ' + fmt_pct(out_sort[j].pct_occ) + '<br>Occupied Housing Units: ' + fmt_comma(out_sort[j].occupiedhu));
			tr_vac_pct_lab.push('Vacancy Rate, ' + out_sort[j].year + ': ' + fmt_pct(out_sort[j].pct_vac) + '<br>Vacant Housing Units: ' + fmt_comma(out_sort[j].vacanthu));
 		  if(j > 0){
			 yty.push(out_sort[j].year);
			 tr_tot_yty.push(out_sort[j].totalhu - out_sort[j-1].totalhu);
			 tr_tot_yty_lab.push('Year over Year Difference<br>Total Housing Units ' + out_sort[j-1].year + " to " + out_sort[j].year + ': ' + fmt_comma(out_sort[j].totalhu - out_sort[j-1].totalhu));
	         tr_occ_yty.push(out_sort[j].occupiedhu - out_sort[j-1].occupiedhu);
			 tr_occ_yty_lab.push('Year over Year Difference<br>Occupied Housing Units ' + out_sort[j-1].year + " to " + out_sort[j].year + ': ' + fmt_comma(out_sort[j].occupiedhu - out_sort[j-1].occupiedhu));	         
		     yty_data.push({
				 'fips' : out_sort[j].fips, 
			     'name' : out_sort[j].name, 
				 'previous_year' : out_sort[j -1].year,
			     'current_year' : out_sort[j].year, 
		         'previous_total_hu' : fmt_comma(out_sort[j -1].totalhu),
		         'current_total_hu' : fmt_comma(out_sort[j].totalhu),				  
			     'previous_occupied_hu' : fmt_comma(out_sort[j -1].occupiedhu), 
			   	 'current_occupied_hu' : fmt_comma(out_sort[j].occupiedhu), 
			     'yty_diff_total_hu' : fmt_comma(out_sort[j].totalhu - out_sort[j-1].totalhu),
				 'yty_diff_occupied_hu' : fmt_comma(out_sort[j].occupiedhu - out_sort[j-1].occupiedhu)
				 });
		  }
        } //j
//Prepping the final out_sort data
var out_fin = [];
out_sort.forEach(obj => {
	out_fin.push({
		'fips' : obj.fips, 
		'name' : obj.name, 
		'year' : obj.year, 
		'total_hu' : fmt_comma(obj.totalhu), 
	   'occupied_hu' : fmt_comma(obj.occupiedhu), 
	   'vacant_hu' : fmt_comma(obj.vacanthu),
	   'pct_occ_hu' : fmt_pct(obj.pct_occ),
	   'pct_vac_hu' : fmt_pct(obj.pct_vac)
});
});

//Titles
var tit_str0 = "Total and Occupied Housing Units, " + plName;
var tit_str1 = "Year to Year Difference, " + plName;
var tit_str2 = "Occupied and Vacant Housing Units " + plName;


//Chart 1
	tr_total = {x: yearList,
			y :  tr_total_hu,
			customdata : tr_total_lab,
			hovertemplate : '%{customdata}',
			hoverlabel : {namelength :0},
			name : 'Total Housing Units',
			marker: {color: 'green'},
			mode : 'lines+markers'
			};

	tr_occ = {x: yearList,
			y :  tr_occ_hu,
			customdata : tr_occ_lab,
			hovertemplate : '%{customdata}',
			hoverlabel : {namelength :0},
			name : 'Occupied Housing Units',
			marker: {color: 'blue'},
			mode : 'lines+markers'
			};
 var ch_0 = [tr_total, tr_occ];
 
 //Chart 2
	yty_total = {x: yty,
			y :  tr_tot_yty,
			customdata : tr_tot_yty_lab,
			hovertemplate : '%{customdata}',
			hoverlabel : {namelength :0},
			name : 'Total Housing Units',
			marker: {color: 'green'},
			mode : 'lines+markers'
			};
	
	yty_occ = {x: yty,
			y :  tr_occ_yty,
			customdata : tr_occ_yty_lab,
			hovertemplate : '%{customdata}',
			hoverlabel : {namelength :0},
			name : 'Occupied Housing Units',
			marker: {color: 'blue'},
			mode : 'lines+markers'
			};
 var ch_1 = [yty_total, yty_occ];

//Chart 3
	occ_bar = {x: yearList,
			y :  tr_occ_hu,
			customdata : tr_occ_pct_lab,
			hovertemplate : '%{customdata}',
			hoverlabel : {namelength :0},
			marker: {color: 'blue', opacity : 0.8},
			name : 'Occupied Housing Units',
			type : 'bar'
			};

	vac_bar = {x: yearList,
			y :  tr_vac_hu,
			customdata : tr_vac_pct_lab,
			hovertemplate : '%{customdata}',
			hoverlabel : {namelength :0},
			marker: {color: 'brown', opacity : 0.8},
			name : 'Vacant Housing Units',
			type : 'bar'
			};
 var ch_2 = [occ_bar, vac_bar];
 
//Generating Chart	
var config = {responsive: true,
              displayModeBar: false};
//layouts

  var layout0 = {
		title: tit_str0,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			range: [2009, yrvalue+1],
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			title : 'Housing Units',
			automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};	
		
var layout1 = {
		title: tit_str1,
		  autosize: false,
		  width: 1000,
		  height: 500,
		  xaxis: {
			title : 'Year',
			range: [2009, yrvalue+1],
			showgrid: true,
			zeroline: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2
		  },
		  yaxis: {
			title : 'Housing Units',
			automargin : true,
			showgrid: true,
			showline: true,
			mirror: 'ticks',
			gridcolor: '#bdbdbd',
			gridwidth: 1,
			linecolor: 'black',
			linewidth: 2,
			tickformat: ','
		  },
			annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
		};	
		
var layout2 = {
	barmode: 'stack',  
	title: tit_str2,
	  autosize: false,
	  width: 1000,
	  height: 500,
	  xaxis: {
		title : 'Year',
		range: [2009, yrvalue+1],
		showgrid: true,
		zeroline: true,
		showline: true,
		mirror: 'ticks',
		gridcolor: '#bdbdbd',
		gridwidth: 1,
		linecolor: 'black',
		linewidth: 2
	  },
	  yaxis: {
		title : 'Housing Units',
		automargin : true,
		showgrid: true,
		showline: true,
		mirror: 'ticks',
		gridcolor: '#bdbdbd',
		gridwidth: 1,
		linecolor: 'black',
		linewidth: 2,
		tickformat: ','
	  },
		annotations : [annot('Data and Visualization by the Colorado State Demography Office.')]
	};

		

	
//Clearing out Divs
var CHART0 = document.getElementById("huline_output");
var CHART1 = document.getElementById("yoyline_output");
var CHART2 = document.getElementById("vacantbar_output");

CHART0.innerHTML = "";
CHART1.innerHTML = "";
CHART2.innerHTML = "";

Plotly.newPlot(CHART0, ch_0, layout0, config);
Plotly.newPlot(CHART1, ch_1, layout1, config);
Plotly.newPlot(CHART2, ch_2, layout2, config);

//Link to output buttons
huline_csv.onclick = function() { exportToCsv(plName, 'housing', out_fin, 0); }; 
ytyline_csv.onclick = function() { exportToCsv(plName, 'housing', yty_data, 0); }; 
vacantbar_csv.onclick = function() { exportToCsv(plName, 'housing', out_fin, 0); }; 
 
huline_png.onclick = function() { exportToPng(plName, 'housing', CHART0, 0); }; 
ytyline_png.onclick = function() { exportToPng(plName, 'housing', CHART1, 0); }; 
vacantbar_png.onclick = function() { exportToPng(plName, 'housing', CHART2, 0); }; 


}); //end of Promise

}; 
// genHOUSEDASH

//cat Migration Flows Dashboard functions

function genFLOWYR(inYR){
//genFLOWYR generates the ACS years

	if(inYR > 2020) { inYR = 2020}  //Adjust this to the latest version of the ACS Migration Flows data
	var prevYR = inYR - 4;
	var yrArr = [];
	var YRStr  = String(prevYR) + "-" + String(inYR)
	yrArr.push({"YEAR" : inYR, "ACS_YR" : YRStr})
	var chkYr = inYR
	for(i = inYR; i >= 2010; i--) {
	    if(i == chkYr - 5){
			var prevYR = i - 4;
			var YRStr  = String(prevYR) + "-" + String(i)
	        yrArr.push({"YEAR" : i, "ACS_YR" : YRStr})
			chkYr = i
		}
   }

 return(yrArr)
}  
// genFLOWYR


function parsePhrase (phrase){
//genFLOWYR parsePhrase returns an adjusted total from the summary record

	var outphrase = phrase.split(" ")
	var outval = parseInt(outphrase[3].replaceAll(",",""))
	return(outval)
}
//parsePhrase

function supressData(inData, fips, geo_name, type){
// genFLOWYR and genLODES supressData compresses data sets to have a maximum of 35 entires (20 px per entry in a chart with 700 px) OR entry with 10 or fewer movers

	var fmt_comma = d3.format(",");

	if(geo_name ==""){
		geo_name = countyName(fips)
	}

		switch(type) {
		case 'net' :{
 		  var posdata = inData.filter(d => d.MOVEDNET_EST >= 0)
          var possort = posdata.sort(function(a, b){return d3.descending(a['MOVEDNET_EST'], b['MOVEDNET_EST']); })
		  var poscnt = 0;
		  var posmax = 0;
		  var posfin = [];
          var posphrase = ""
			  for(i = 0; i < possort.length; i++) {
				  if(i < 20){
					  posfin[i] = possort[i];
				  } else {
				      poscnt++
				      posmax =  posmax + possort[i].MOVEDNET_EST;
					  if(poscnt == 1){
					  var posphrase = fmt_comma(poscnt) + ' location with ' + fmt_comma(posmax)+ ' movers'
					  } else {
					  var posphrase = fmt_comma(poscnt) + ' locations with ' + fmt_comma(posmax)+ ' movers'
					  }  
			      }
			  }


		//adding record for supression
		 if(poscnt > 0 && posmax > 0){
			  posfin.push({
				"GEOID1" : "",
				"GEOID2" : "",
				"NAME1" : geo_name,
				"STATE1" : "",
				"NAME2" : posphrase.length > 0 ? posphrase : "",
				"STATE2" : "",
				"MOVEDIN_EST" : 0,
				"MOVEDIN_MOE" : 0,
				"MOVEDOUT_EST" : 0,
				"MOVEDOUT_MOE" : 0,
				"MOVEDNET_EST" : 10,
				"MOVEDNET_MOE" : 0 })
			  }
			  
		  var negdata = inData.filter(d => d.MOVEDNET_EST < 0)

          var negsort = negdata.sort(function(a, b){return d3.ascending(a['MOVEDNET_EST'], b['MOVEDNET_EST']); })

		  var negcnt = 0;
		  var negmax = 0;
		  var negfin = [];
		  var negphrase = "";
			  for(i = 0; i < negsort.length; i++) {
				  if(i < 20){
					  negfin[i] = negsort[i];
				  } else {
				      negcnt++
				      negmax =  negmax + Math.abs(negsort[i].MOVEDNET_EST);
					  if(negcnt == 1){
							var negphrase = fmt_comma(negcnt) + ' location with ' + fmt_comma(Math.abs(negmax))+ ' movers';
						} else {
							var negphrase = fmt_comma(negcnt) + ' locations with ' + fmt_comma(Math.abs(negmax))+ ' movers';
						}
			      }
			  }
		
				//adding record for supression
		 if(negcnt > 0 && negmax > 0){
			  negfin.push({
				"GEOID1" : "",
				"GEOID2" : "",
				"NAME1" : geo_name,
				"STATE1" : "",
				"NAME2" : negphrase.length > 0 ? negphrase : "",
				"STATE2" : "",
				"MOVEDIN_EST" : 0,
				"MOVEDIN_MOE" : 0,
				"MOVEDOUT_EST" : 0,
				"MOVEDOUT_MOE" : 0,
				"MOVEDNET_EST" : -10,
				"MOVEDNET_MOE" : 0 })
			  }
		var outdata = posfin.concat(negfin);
		break;
		}  //net
	case "in" : {
          var insort = inData.sort(function(a, b){return d3.descending(a['MOVEDIN_EST'], b['MOVEDIN_EST']); })
		  var incnt = 0;
		  var inmax = 0;
		  var infin = [];
		  var inphrase = "";
			  for(i = 0; i < insort.length; i++) {
				  if(i < 20){
					  infin[i] = insort[i];
				  } else {
				      incnt++
				      inmax =  inmax + insort[i].MOVEDIN_EST;
					  if(incnt == 1){
					  var inphrase = fmt_comma(incnt) + ' location with ' + fmt_comma(inmax)+ ' movers'
					  } else {
						  var inphrase = fmt_comma(incnt) + ' locations with ' + fmt_comma(inmax)+ ' movers'
					  }
			      }
			  }
	
		 //adding record for supression
		 if(incnt > 0 && inmax > 0){
			  infin.push({
				"GEOID1" : "",
				"GEOID2" : "",
				"NAME1" : geo_name,
				"STATE1" : "",
				"NAME2" : inphrase.length > 0 ? inphrase : "",
				"STATE2" : "",
				"MOVEDIN_EST" : 10,
				"MOVEDIN_MOE" : 0,
				"MOVEDOUT_EST" : 0,
				"MOVEDOUT_MOE" : 0,
				"MOVEDNET_EST" : 0,
				"MOVEDNET_MOE" : 0 })
			  }
		var outdata = infin;
		break;
	} //in
	case "out" : {
          var outsort = inData.sort(function(a, b){return d3.descending(a['MOVEDOUT_EST'], b['MOVEDOUT_EST']); })
		  var outcnt = 0;
		  var outmax = 0;
		  var outfin = [];
		  var outphrase = "";
			  for(i = 0; i < outsort.length; i++) {
				  if(i < 20){
					  outfin[i] = outsort[i];
				  } else {
				      outcnt++
				      outmax =  outmax + outsort[i].MOVEDOUT_EST;
					  if(outcnt == 1){
					  var outphrase = fmt_comma(outcnt) + ' location with ' + fmt_comma(outmax)+ ' movers'
					  } else {
						  var outphrase = fmt_comma(outcnt) + ' locations with ' + fmt_comma(outmax)+ ' movers'
					  }
			      }
			  }
	
		//adding record for supression
		 if(outcnt > 0 && outmax > 0){
			  outfin.push({
				"GEOID1" : "",
				"GEOID2" : "",
				"NAME1" : geo_name,
				"STATE1" : "",
				"NAME2" : outphrase.length > 0 ? outphrase : "",
				"STATE2" : "",
				"MOVEDIN_EST" : 0,
				"MOVEDIN_MOE" : 0,
				"MOVEDOUT_EST" : 10,
				"MOVEDOUT_MOE" : 0,
				"MOVEDNET_EST" : 0,
				"MOVEDNET_MOE" : 0 })
			  }
		var outdata = outfin;
		break;
	} //out
	case 'lodes' :{
 		  var posdata = inData.filter(d => d.jobs >= 0)
          var possort = posdata.sort(function(a, b){return d3.descending(a['jobs'], b['jobs']); })
		  var poscnt = 0;
		  var posmax = 0;
		  var postmp = [];
          var posphrase = ""
			  for(i = 0; i < possort.length; i++) {
				  if(i < 21){
					  postmp[i] = possort[i];
				  } else {
				      poscnt++
				      posmax =  posmax + possort[i].jobs;
			      }
			  }

		//adding record for supression
		 if(posdata.length > 20){
			  postmp.push({
				"residential_location" : fmt_comma(poscnt) + ' locations with ' + fmt_comma(Math.abs(posmax))+ ' workers',
				"work_location" : geo_name,
				"jobs" : posmax
				})
			  }
  
		  var negdata = inData.filter(d => d.jobs < 0)
          var negsort = negdata.sort(function(a, b){return d3.ascending(a['jobs'], b['jobs']); })
		  var negcnt = 0;
		  var negmax = 0;
		  var negtmp = [];
		  var negphrase = "";
			  for(i = 0; i < negsort.length; i++) {
				  if(i < 21){
					  negtmp[i] = negsort[i];
				  } else {
				      negcnt++
				      negmax =  negmax + Math.abs(negsort[i].jobs);
			      }
			  }
		
		//adding record for supression
		 if(negdata.length > 20){
			  negtmp.push({
				"residential_location" : geo_name,
				"work_location" : fmt_comma(negcnt) + ' locations with ' + fmt_comma(Math.abs(negmax))+ ' workers',
				"jobs" : negmax })
			  }
			  
		var outdata = postmp.concat(negtmp);
		break;
		}  //lodes
	} //switch
return(outdata)
} 
// supressData

function genFLOWS(fips, name, yearval){
// genFLOWS Generates Migration Flows Dashboard

	var fmt_comma = d3.format(",");
	const fmt_date = d3.timeFormat("%B %d, %Y");
	var CHART0 = document.getElementById("net_output");
	var CHART1 = document.getElementById("in_output");
	var CHART2 = document.getElementById("out_output");

	CHART0.innerHTML = "";
	CHART1.innerHTML = "";
	CHART2.innerHTML = "";
	
	var plname = countyName(parseInt(fips))
	var citval = parseInt(yearval) + 1
	var titleVal_net = plname + " Net Migration " + (yearval - 4) + "-" + yearval;
	var titleVal_out = plname + " Out Migration " +  (yearval - 4) + "-" + yearval;
	var titleVal_in = plname + " In Migration " + (yearval - 4) + "-" + yearval;
	var citStr = "U.S. Census Bureau ("+ citval + ") County to County Migration Flows " + (yearval - 4) + "-" + yearval +
	           "<br>Print Date: "+ fmt_date(new Date);

	var flowcall = "/acs/flows?get=GEOID1,GEOID2,FULL1_NAME,FULL2_NAME,MOVEDIN,MOVEDIN_M,MOVEDOUT,MOVEDOUT_M,MOVEDNET,MOVEDNET_M"
	var censKey = '08fe07c2a7bf781b7771d7cccb264fe7ff8965ce'
	if(fips == "000") {
		var urlstr = "https://api.census.gov/data/" + yearval + flowcall + "&for=county:*&in=state:08&key=" + censKey;
	} else {
		var urlstr = "https://api.census.gov/data/" + yearval + flowcall + "&for=county:" + fips + "&in=state:08&key=" + censKey;
	}


//Reading data
   d3.json(urlstr).then(function(data){
	 var outdata = [];

	 for(i = 1; i < data.length; i++){
	 
	 if(data[i][2].indexOf(",") > -1) {
	     var namein = data[i][2].split(",")
	 } else {
		 var namein = [data[i][2]]
	 }
	 if(data[i][3].indexOf(",") > -1) {
	     var nameout = data[i][3].split(",")
		 if(nameout.length == 3){
			 nameout.shift()
		 }
	 } else {
		 var nameout = [data[i][3]]
	 }

	 if(!!data[i][1]){ //GEOID2 is not NULL
	    var chkval = data[i][1].toString();
	 } else {
		var chkval = "";
	 }
	 if(chkval.length <= 5){
	  outdata.push({
		 "GEOID1" : data[i][0],
		 "GEOID2" : data[i][1],
		  "NAME1" : namein.length == 2 ? namein[0].trim() : "",
		  "STATE1" : namein.length == 2 ? namein[1].trim() : namein[0].trim(),
		  "NAME2" : nameout.length == 2 ? nameout[0].trim() : "",
		  "STATE2" : nameout.length == 2 ? nameout[1].trim() : nameout[0].trim(),
	      "MOVEDIN_EST" : data[i][4] == null ? 0 : parseInt(data[i][4]),
		  "MOVEDIN_MOE" : data[i][5] == null ? 0 : parseInt(data[i][5]),
		  "MOVEDOUT_EST" : data[i][6] == null ? 0 : parseInt(data[i][6]),
		  "MOVEDOUT_MOE" : data[i][7] == null ? 0 : parseInt(data[i][7]),
		  "MOVEDNET_EST" : data[i][8] == null ? null :parseInt(data[i][8]),
		  "MOVEDNET_MOE" : data[i][9] == null ? null :parseInt(data[i][9])
	  })
	 }
	 } //for
	 

//Prepping data for summarization
var sumdata = []
outdata.forEach(d => {
	sumdata.push({
		 "GEOID1" : d.GEOID1,
		 "GEOID2" : d.GEOID2,
		 "NAME1" : d.NAME1,
		 "STATE1" : d.STATE1,
		 "NAME2" : d.NAME2,
		 "STATE2" : d.STATE2,
	     "MOVEDIN_EST" : d.MOVEDIN_EST,
		 "MOVEDIN_MOE" : Math.pow(d.MOVEDIN_MOE,2),
		 "MOVEDOUT_EST" : d.MOVEDOUT_EST,
		 "MOVEDOUT_MOE" : Math.pow(d.MOVEDOUT_MOE,2), 
		 "MOVEDNET_EST" : d.MOVEDNET_EST,
		 "MOVEDNET_MOE" : Math.pow(d.MOVEDNET_MOE,2)
	})
})

var sumdata2 = sumdata.filter(d => d.MOVEDNET_EST !== null)

//summarizing state data and summarizing out-of state movement for other counties

var columnsToSum = ["MOVEDIN_EST", "MOVEDIN_MOE", "MOVEDOUT_EST", "MOVEDOUT_MOE", "MOVEDNET_EST", "MOVEDNET_MOE"] 
var bindata = [];
if(fips == "000"){
	    var binroll =  d3.rollup(sumdata2, v => Object.fromEntries(columnsToSum.map(col => [col, d3.sum(v, d => +d[col])])), d => d.STATE1, d => d.STATE2);
		for (let [key1, value] of binroll) {
		for (let[key2, value2] of value) {
		   bindata.push({'NAME1' : key1, 'STATE1' : '', 'NAME2' : key2, 'STATE2' : '',
			 'MOVEDIN_EST' : value2.MOVEDIN_EST, 'MOVEDIN_MOE' : Math.sqrt(value2.MOVEDIN_MOE), 
			 'MOVEDOUT_EST' : value2.MOVEDOUT_EST, 'MOVEDOUT_MOE' : Math.sqrt(value2.MOVEDOUT_MOE), 
			 'MOVEDNET_EST' : value2.MOVEDNET_EST, 'MOVEDNET_MOE' : Math.sqrt(value2.MOVEDNET_MOE)});
		};
		};
		var outchartun = bindata.filter(obj => obj.NAME1 != obj.NAME2)
} else {  //out of state 

  var outofstate = sumdata2.filter(obj => obj.STATE2 != "Colorado")
  var binroll =  d3.rollup(outofstate, v => Object.fromEntries(columnsToSum.map(col => [col, d3.sum(v, d => +d[col])])), d => d.NAME1, d => d.STATE1, d => d.STATE2);;
	for (let [key1, value] of binroll) {
	for (let[key2, value2] of value) {
	for (let[key3, value3] of value2) {
		if(value3.MOVEDNET_EST >= 0){
		bindata.push({'NAME1' : key1, 'STATE1' : key2, 'NAME2' : key3,'STATE2' : '',
			 'MOVEDIN_EST' : value3.MOVEDIN_EST, 'MOVEDIN_MOE' : Math.sqrt(value3.MOVEDIN_MOE), 
			 'MOVEDOUT_EST' : value3.MOVEDOUT_EST, 'MOVEDOUT_MOE' : Math.sqrt(value3.MOVEDOUT_MOE), 
			 'MOVEDNET_EST' : value3.MOVEDNET_EST, 'MOVEDNET_MOE' : Math.sqrt(value3.MOVEDNET_MOE)});
		}
		}
		};
		};
  var instate = sumdata2.filter(obj => obj.STATE2 == "Colorado");
  var instate_sum = [];
  instate.forEach( d => {
		instate_sum.push({
		 "GEOID1" : d.GEOID1,
		 "GEOID2" : d.GEOID2,
		 "NAME1" : d.NAME1,
		 "STATE1" : d.STATE1,
		 "NAME2" : d.NAME2,
		 "STATE2" : d.STATE2,
	     "MOVEDIN_EST" : d.MOVEDIN_EST,
		 "MOVEDIN_MOE" : Math.sqrt(d.MOVEDIN_MOE),
		 "MOVEDOUT_EST" : d.MOVEDOUT_EST,
		 "MOVEDOUT_MOE" : Math.sqrt(d.MOVEDOUT_MOE), 
		 "MOVEDNET_EST" : d.MOVEDNET_EST,
		 "MOVEDNET_MOE" : Math.sqrt(d.MOVEDNET_MOE)})
  })
  
	var outchartun = instate_sum.concat(bindata);
}
var outchart_net = supressData(outchartun, fips, "", "net")
var outchart_in = supressData(outchartun, fips, "", "in")
var outchart_out = supressData(outchartun, fips, "","out")

// Creating Nodeslist
var nodeslist_net = [];
outchart_net.forEach(obj => { 
	nodeslist_net.push({'location1' : obj.NAME1, 'location2' : obj.NAME2,  'value' : obj.MOVEDNET_EST})
})

var labarr_net = [];
//labarr_net.push(nodeslist_net[0].location1);
labarr_net.push("");
for(i = 0; i < nodeslist_net.length; i++){
	labarr_net.push(nodeslist_net[i].location2);
}
		

var neg = nodeslist_net.filter(d => d.value <= 0).length
var pos = nodeslist_net.filter(d => d.value > 0).length

if(neg < pos) {
	var inc = 1/pos;
} else {
	var inc = 1/neg;
}
var incr = parseFloat(inc.toFixed(3))


// Prepping _net migration data

var total_pos_netmig = 0;
var total_neg_netmig = 0;
var y_net_pos = 0.1;
var y_net_neg = 0.12;

// Prepping _net migration data


for(i = 0; i < nodeslist_net.length;i++){
		if(nodeslist_net[i].value < 0) {
			nodeslist_net[i].src = labarr_net.indexOf(nodeslist_net[i].location1)
			nodeslist_net[i].tgt = labarr_net.indexOf(nodeslist_net[i].location2)
			nodeslist_net[i].val = Math.abs(nodeslist_net[i].value)
			if(nodeslist_net[i].location2.includes("movers")){
				nodeslist_net[i].lablink = nodeslist_net[i].location2;
			} else {
				nodeslist_net[i].lablink = nodeslist_net[i].location1 + " to " + nodeslist_net[i].location2 + ": " + fmt_comma(Math.abs(nodeslist_net[i].value));	
			}
			nodeslist_net[i].xpos =  0.9;
			nodeslist_net[i].labposx = 0.95;
			nodeslist_net[i].labposy = 1 - parseFloat(y_net_neg.toFixed(3));
			nodeslist_net[i].lab = nodeslist_net[i].location2;
			nodeslist_net[i].ypos =  parseFloat(y_net_neg.toFixed(3));
			if(nodeslist_net[i].location2.includes("movers")){
				total_neg_netmig = total_neg_netmig + Math.abs(parsePhrase(nodeslist_net[i].location2));
			} else {
			     total_neg_netmig = total_neg_netmig + Math.abs(nodeslist_net[i].value);
			}
			y_net_neg = y_net_neg + incr;
		} else {
			nodeslist_net[i].src = labarr_net.indexOf(nodeslist_net[i].location2)
			nodeslist_net[i].tgt = labarr_net.indexOf(nodeslist_net[i].location1)
			nodeslist_net[i].val = Math.abs(nodeslist_net[i].value)

			
			if(nodeslist_net[i].location2.includes("movers")){
				nodeslist_net[i].lablink = nodeslist_net[i].location2;
			} else {
				nodeslist_net[i].lablink = nodeslist_net[i].location2 + " to " + nodeslist_net[i].location1 + ": " + fmt_comma(Math.abs(nodeslist_net[i].value));	
			}
			nodeslist_net[i].xpos =  0.1;
			nodeslist_net[i].labposx = -0.05;
			nodeslist_net[i].labposy = 1 - parseFloat(y_net_neg.toFixed(3));
			nodeslist_net[i].lab = nodeslist_net[i].location2;
			nodeslist_net[i].lab = nodeslist_net[i].location2;
			nodeslist_net[i].ypos =  parseFloat(y_net_pos.toFixed(3));
			if(nodeslist_net[i].location2.includes("movers")){
				total_pos_netmig = total_pos_netmig + parsePhrase(nodeslist_net[i].location2);
			} else {
			     total_pos_netmig = total_pos_netmig + nodeslist_net[i].value;
			}
			y_net_pos = y_net_pos + incr;
		}
		if(nodeslist_net[i].tgt == -1) { nodeslist_net[i].tgt = 0}
		if(nodeslist_net[i].src == -1) { nodeslist_net[i].src = 0}
} //i
var nodes_1 = []
nodes_1.push({"location1" : countyName(parseInt(fips)),
		"location2" : countyName(parseInt(fips)),
		"value" : 0,
		"src" :0,
		"tgt" : 0,
		"val" : 0,
        "xpos" : 0.5,
        "ypos" : 0.5,
		"lablink" : ""	  
})
	 
var nodeslist_net = nodes_1.concat(nodeslist_net)

//Build Label Annotations
var lab_annotation = [];
nodeslist_net.forEach( d => {
	lab_annotation.push({text: d.lab,
		font : {size : 11, color : 'black'},      
        x : d.labposx,
        y : d.labposy,
		showarrow : false})
})


//Net Migration Plot

var net_in_mig_lab = "Net In Migration: "+ fmt_comma(total_pos_netmig);
var net_out_mig_lab = "Net Out Migration: "+ fmt_comma(total_neg_netmig);

var data_net = {
  type: "sankey",
  orientation: "h",
  arrangement : "fixed",
  node: {
    thickness: 30,
    line: {
      color: "black",
      width: 0.5
    },
   label: nodeslist_net.map(d => d.lab),
   x : nodeslist_net.map(d => d.xpos),
   y : nodeslist_net.map(d => d.ypos),
   pad : 35,
   hoverinfo: 'none'
      },

  link: {
    source: nodeslist_net.map(d => d.src),
    target: nodeslist_net.map(d => d.tgt),
    value:  nodeslist_net.map(d => d.val),
	customdata : nodeslist_net.map(d => d.lablink),
	hovertemplate : '%{customdata}<extra></extra>'
  }
}

var data_netp = [data_net];

var layout_net = {
  title: titleVal_net, autosize : false, 
  width: 840,
  height: 700,
  font: {
    size: 11,
	family : 'Arial Black'
  },
annotations : [
      {text :  citStr , 
      font: { size : 9, color: 'black'},
      xref : 'paper', 
	  yref : 'paper', 
	  xanchor : 'left',
	  yanchor : 'bottom',
      x : 0, 
      y : -0.135, 
      align : 'left', 
      showarrow : false},
	  {text : net_in_mig_lab,
        font : {size : 10, color : 'black'},
        xref : 'paper', 
	    yref : 'paper', 
	    xanchor : 'left',
	    yanchor : 'bottom',
	    x : 0,
        y : 1,
		showarrow : false },
		{text : net_out_mig_lab,
        font : {size : 10, color : 'black'},      
		xref : 'paper', 
	    yref : 'paper', 
	    xanchor : 'left',
	    yanchor : 'bottom',
        x : 0.75,
        y : 1,
		showarrow : false }]
}

// Prepping in migration data
var nodeslist_in = [];
outchart_in.forEach(obj => { 
	nodeslist_in.push({'location1' : obj.NAME1, 'location2' : obj.NAME2,  'value' : obj.MOVEDIN_EST})
})

var labarr_in = [];
labarr_in.push("");
for(i = 0; i < nodeslist_in.length; i++){
	labarr_in.push(nodeslist_in[i].location2);
}
		
	var inc = 1/nodeslist_in.length;

var incr = parseFloat(inc.toFixed(3))


// Prepping _in migration data

var total_pos_inmig = 0;
var y_in_pos = 0.1;

// Prepping _in migration data
for(i = 0; i < nodeslist_in.length;i++){
			nodeslist_in[i].src = labarr_in.indexOf(nodeslist_in[i].location2)
			nodeslist_in[i].tgt = labarr_in.indexOf(nodeslist_in[i].location1)
			nodeslist_in[i].val = Math.abs(nodeslist_in[i].value)
			if(nodeslist_in[i].location2.includes("movers")){
				nodeslist_in[i].lablink = nodeslist_in[i].location2;
			} else {
				nodeslist_in[i].lablink = nodeslist_in[i].location2 + " to " + nodeslist_in[i].location1 + ": " + fmt_comma(Math.abs(nodeslist_in[i].value));	
			}			
			nodeslist_in[i].xpos =  0.1;
			nodeslist_in[i].ypos =  parseFloat(y_in_pos.toFixed(3));
			nodeslist_in[i].lab = nodeslist_in[i].location2
			if(nodeslist_in[i].location2.includes("movers")){
				total_pos_inmig = total_pos_inmig + parsePhrase(nodeslist_in[i].location2);
			} else {
			     total_pos_inmig = total_pos_inmig + nodeslist_in[i].value;
			}
			y_in_pos = y_in_pos + incr;
	
		if(nodeslist_in[i].tgt == -1) { nodeslist_in[i].tgt = 0}
		if(nodeslist_in[i].src == -1) { nodeslist_in[i].src = 0}
} //i
var nodes_1 = []
nodes_1.push({"location1" : countyName(parseInt(fips)),
		"location2" : countyName(parseInt(fips)),
		"value" : 0,
		"src" :0,
		"tgt" : 0,
		"val" : 0,
        "xpos" : 0.5,
        "ypos" : 0.5,
		"lablink" : ""	  
})
	 
var nodeslist_in = nodes_1.concat(nodeslist_in)

//in Migration Plot

var in_in_mig_lab = "In Migration: "+ fmt_comma(total_pos_inmig);

var data_in = {
  type: "sankey",
  orientation: "h",
  arrangement : "fixed",
  node: {
    thickness: 30,
    line: {
      color: "black",
      width: 0.5
    },
   label: nodeslist_in.map(d => d.lab),
   x : nodeslist_in.map(d => d.xpos),
   y : nodeslist_in.map(d => d.ypos),
   pad : 35,
   hoverinfo: 'none'
      },

  link: {
 source: nodeslist_in.map(d => d.src),
    target: nodeslist_in.map(d => d.tgt),
    value:  nodeslist_in.map(d => d.val),
	customdata : nodeslist_in.map(d => d.lablink),
	hovertemplate : '%{customdata}<extra></extra>'
  }
}

var data_inp = [data_in];

var layout_in = {
  title: titleVal_in, autosize: false,
  width: 840, 
  height: 700, 
  font: {
    size: 11,
	family : 'Arial Black'
  },
annotations : [{text :  citStr , 
      font: { size : 9, color: 'black'},
      xref : 'paper', 
	  yref : 'paper', 
	  xanchor : 'left',
	  yanchor : 'bottom',
      x : 0.5, 
      y : 0, 
      align : 'left', 
      showarrow : false},
	  {text : in_in_mig_lab,
        font : {size : 10, color : 'black'},
        xref : 'paper', 
	    yref : 'paper', 
	    xanchor : 'left',
	    yanchor : 'bottom',
	    x : 0.75,
        y : 1,
		showarrow : false }]
}
  
// Prepping out migration data
var nodeslist_out = [];
var nodesuniq_out = [];
outchart_out.forEach(obj => { 
	nodeslist_out.push({'location1' : obj.NAME1, 'location2' : obj.NAME2,  'value' : obj.MOVEDOUT_EST})
})

var labarr_out = [];
labarr_out.push("");
for(i = 0; i < nodeslist_out.length; i++){
	labarr_out.push(nodeslist_out[i].location2);
}
		
	var inc = 1/nodeslist_out.length;

var incr = parseFloat(inc.toFixed(3))

// Prepping _out migration data

var total_pos_outmig = 0;
var y_out_pos = 0.1;

// Prepping _out migration data

for(i = 0; i < nodeslist_out.length;i++){
			nodeslist_out[i].src = labarr_out.indexOf(nodeslist_out[i].location1)
			nodeslist_out[i].tgt = labarr_out.indexOf(nodeslist_out[i].location2)
			nodeslist_out[i].val = Math.abs(nodeslist_out[i].value)
			if(nodeslist_out[i].location2.includes("movers")){
				nodeslist_out[i].lablink = nodeslist_out[i].location2;
			} else {
				nodeslist_out[i].lablink = nodeslist_out[i].location1 + " to " + nodeslist_out[i].location2 + ": " + fmt_comma(nodeslist_out[i].value);	
			}
			nodeslist_out[i].xpos =  0.9;
			nodeslist_out[i].ypos =  parseFloat(y_out_pos.toFixed(3));
			nodeslist_out[i].lab = nodeslist_out[i].location2;
			if(nodeslist_out[i].location2.includes("movers")){
				total_pos_outmig = total_pos_outmig + parsePhrase(nodeslist_out[i].location2);
			} else {
			     total_pos_outmig = total_pos_outmig + nodeslist_out[i].value;
			}			
			y_out_pos = y_out_pos + incr;
		if(nodeslist_out[i].tgt == -1) { nodeslist_out[i].tgt = 0}
		if(nodeslist_out[i].src == -1) { nodeslist_out[i].src = 0}
} //i
var nodes_1 = []
nodes_1.push({"location1" : countyName(parseInt(fips)),
		"location2" : countyName(parseInt(fips)),
		"value" : 0,
		"src" :0,
		"tgt" : 0,
		"val" : 0,
        "xpos" : 0.5,
        "ypos" : 0.5,
		"lablink" : ""	  
})
	 
var nodeslist_out = nodes_1.concat(nodeslist_out)
//out Migration Plot

var out_out_mig_lab = "Out Migration: "+ fmt_comma(total_pos_outmig);

var data_out = {
  type: "sankey",
  orientation: "h",
  arrangement : "fixed",
  node: {
    thickness: 30,
    line: {
      color: "black",
      width: 0.5
    },
   label: nodeslist_out.map(d => d.lab),
   x : nodeslist_out.map(d => d.xpos),
   y : nodeslist_out.map(d => d.ypos),
   pad : 35,
   hoverinfo: 'none'
      },

  link: {
 source: nodeslist_out.map(d => d.src),
    target: nodeslist_out.map(d => d.tgt),
    value:  nodeslist_out.map(d => d.val),
	customdata : nodeslist_out.map(d => d.lablink),
	hovertemplate : '%{customdata}<extra></extra>'
  }
}

var data_outp = [data_out];

var layout_out = {
  title: titleVal_out, autosize : false,
  width: 840,
  height: 700, 
  font: {
    size: 11,
	family : 'Arial Black'
  },
annotations : [{text :  citStr , 
      font: { size : 9, color: 'black'},
      xref : 'paper', 
	  yref : 'paper', 
	  xanchor : 'left',
	  yanchor : 'bottom',
      x : 0, 
      y : 0, 
      align : 'left', 
      showarrow : false},
		{text : out_out_mig_lab,
        font : {size : 10, color : 'black'},      
		xref : 'paper', 
	    yref : 'paper', 
	    xanchor : 'left',
	    yanchor : 'bottom',
        x : 0,
        y : 1,
		showarrow : false }]
}

//Plotting
Plotly.newPlot(CHART0, data_netp, layout_net);
Plotly.newPlot(CHART1, data_inp, layout_in);
Plotly.newPlot(CHART2, data_outp, layout_out);

//Export Code
var chartnet_csv = document.getElementById('net_csv');
var chartnet_png = document.getElementById('net_png');
chartnet_csv.onclick = function() {exportToCsv(plname, 'netflow', outchartun,0)};
chartnet_png.onclick = function() {exportToPng(plname, 'netflow', CHART0,0)};

var chartin_csv = document.getElementById('in_csv');
var chartin_png = document.getElementById('in_png');
chartin_csv.onclick = function() {exportToCsv(plname, 'inflow', outchartun,0)};
chartin_png.onclick = function() {exportToPng(plname, 'inflow', CHART1,0)};

var chartout_csv = document.getElementById('out_csv');
var chartout_png = document.getElementById('out_png');
chartout_csv.onclick = function() {exportToCsv(plname, 'outflow', outchartun,0)};
chartout_png.onclick = function() {exportToPng(plname, 'outflow', CHART2,0)};
   }) //promise
} 
//genFLOWS

function lodes_tab(indata,citation,type){
	var labs = [];
	var head_str = "<thead><tr><th align='center' style='word-wrap: break-word'>Location</th>" +
                   "<th align='center' style='word-wrap: break-word'>Number of Jobs</th>" +
                   "<th align='center' style='word-wrap: break-word'>Median Travel Distance (Miles)</th>" +
                   "<th align='center' style='word-wrap: break-word'>5 or fewer Miles</th>" + 
                   "<th align='center' style='word-wrap: break-word'>6 to 10 Miles</th>" + 
                   "<th align='center' style='word-wrap: break-word'>11 to 20 Miles</th>" + 
                   "<th align='center' style='word-wrap: break-word'>21 to 50 Miles</th>" +
                   "<th align='center' style='word-wrap: break-word'>51 to 100 Miles</th>" +
                   "<th align='center' style='word-wrap: break-word'>Greater than 100 Miles</th></tr></thead>"
				   
var tab_str = ""

for(i = 0; i < indata.length;i++){
	if(type == "work"){
		var loc_str = indata[i].residential_location
	} else {
		var loc_str = indata[i].work_location
	}
	var tmp_str = "<tr><td align='left'>" + loc_str + "</td>" +
			  "<td align='right'>" + fixNUMFMT(Math.abs(indata[i].jobs),"num") + "</td>" +
				"<td align='right'>" + fixNUMFMT(indata[i].median_distance,"num") + "</td>" +
				"<td align='right'>" + fixNUMFMT(indata[i].miles_05,"num") + "</td>" + 
				"<td align='right'>" + fixNUMFMT(indata[i].miles_10,"num") + "</td>" +
				"<td align='right'>" + fixNUMFMT(indata[i].miles_20,"num") + "</td>" +
				"<td align='right'>" + fixNUMFMT(indata[i].miles_50,"num") + "</td>" +
				"<td align='right'>" + fixNUMFMT(indata[i].miles_100,"num") + "</td>" +
				"<td align='right'>" + fixNUMFMT(indata[i].miles_gt100,"num") + "</td></tr>"
	var tab_str = tab_str.concat(tmp_str)
	labs.push({'title' : loc_str , 'URL_link' : 'https://onthemap.ces.census.gov/'});
	}

var ftr_str = "<tfoot><tr><td colspan='9'>"+citation+"</td></tr></tfoot>"
var out_str = head_str.concat(tab_str, ftr_str)
var fin_str = out_str.replaceAll("NaN","")
return([fin_str,labs,ftr_str])
}

function genLODES(geo, loc, geo_name, year, sector, CHART0, CHART1, TAB0, TAB1){
// genLODES Generates LODES Dashboard
// this current version pulls data from 2021 LODES data, still considering if future versions will allow year selection

	var fmt_comma = d3.format(",");
	const fmt_date = d3.timeFormat("%B %d, %Y");
	//Initializing screen
	 CHART0.innerHTML = ''
	 CHART1.innerHTML = ''
	 TAB0.innerHTML = ''
	 TAB1.innerHTML = ''

	var fips_code = "08" + loc;
	if(geo == 'county') {
		var geostr = 'county';
    } else {
	  var geostr = 'place'
	}
	//creating barchart diagram data pull
	var barchartstr = 'https://gis.dola.colorado.gov/lookups/lodes?geo=' + geostr + '&geonum=' + fips_code + '&sector=' + sector +'&year=2021&choice=summary'
	var sankeystr = 'https://gis.dola.colorado.gov/lookups/lodes?geo=' + geostr + '&geonum=' + fips_code + '&sector=' + sector + '&year=2021&choice=place'
	
	var prom = [d3.json(barchartstr),d3.json(sankeystr)];
	
	Promise.all(prom).then(function(data){
   
		//Creating analysis data 
		var barchart_data = [];
		var sankey_data = [];
		switch(sector){
		case 'total' :
		 var barchart_title = geo_name + " All Jobs, " + year;
		 for (i = 0; i < data[0].length; i++) {
			barchart_data.push({"work_in_home_in" : parseInt(data[0][i].work_in_home_in_total),
							"work_in_home_out" : parseInt(data[0][i].work_in_home_out_total),
							"work_out_home_in" : parseInt(data[0][i].work_out_home_in_total)
			})
		 }
		 for (i = 0; i < data[1].length; i++) {
				if(geo == 'county'){
					sankey_data.push({"work_st" : data[1][i].work_cty.substr(0,2),
									"work_loc" : data[1][i].work_cty,
									"work_loc_name" : data[1][i].work_cty_name,
									"home_st" : data[1][i].home_cty.substr(0,2),
									"home_loc" : data[1][i].home_cty,
									"home_loc_name" : data[1][i].home_cty_name,
									"jobs" : parseInt(data[1][i].total_jobs),
									"miles_05": parseInt(data[1][i].miles_05),
									"miles_10": parseInt(data[1][i].miles_10),
									"miles_20": parseInt(data[1][i].miles_20),
									"miles_50": parseInt(data[1][i].miles_50),
									"miles_100": parseInt(data[1][i].miles_100),
									"miles_gt100": parseInt(data[1][i].miles_gt100),
									"median_distance":Math.round(parseFloat(data[1][i].median_distance),2)
									})
				} else {
					sankey_data.push({"work_st" : data[1][i].work_place.substr(0,2),
						             "work_loc" : data[1][i].work_place,
									 "work_loc_name" : data[1][i].work_place_name,
									 "home_st" : data[1][i].home_place.substr(0,2),
									 "home_loc" : data[1][i].home_place,
									 "home_loc_name" : data[1][i].home_place_name,
									 "jobs" : parseInt(data[1][i].total_jobs),
									"miles_05": parseInt(data[1][i].miles_05),
									"miles_10": parseInt(data[1][i].miles_10),
									"miles_20": parseInt(data[1][i].miles_20),
									"miles_50": parseInt(data[1][i].miles_50),
									"miles_100": parseInt(data[1][i].miles_100),
									"miles_gt100": parseInt(data[1][i].miles_gt100),
									"median_distance":Math.round(parseFloat(data[1][i].median_distance),2)
									})
				}
		 }
		break;
		case 'goods' :
		 var barchart_title = geo_name + " Goods Producing Jobs, " + year;
		 for (i = 0; i < data[0].length; i++) {
			barchart_data.push({"work_in_home_in" : parseInt(data[0][i].work_in_home_in_goods),
							"work_in_home_out" : parseInt(data[0][i].work_in_home_out_goods),
							"work_out_home_in" : parseInt(data[0][i].work_out_home_in_goods)
			})
		 }
		 for (i = 0; i < data[1].length; i++) {
				if(geo == 'county'){
					sankey_data.push({"work_st" : data[1][i].work_cty.substr(0,2),
									  "work_loc" : data[1][i].work_cty,
									 "work_loc_name" : data[1][i].work_cty_name,
									 "home_st" : data[1][i].home_cty.substr(0,2),
									 "home_loc" : data[1][i].home_cty,
									 "home_loc_name" : data[1][i].home_cty_name,
									 "jobs" : parseInt(data[1][i].goods),
									"miles_05": parseInt(data[1][i].miles_05),
									"miles_10": parseInt(data[1][i].miles_10),
									"miles_20": parseInt(data[1][i].miles_20),
									"miles_50": parseInt(data[1][i].miles_50),
									"miles_100": parseInt(data[1][i].miles_100),
									"miles_gt100": parseInt(data[1][i].miles_gt100),
									"median_distance":Math.round(parseFloat(data[1][i].median_distance),2)
									})
				} else {
					sankey_data.push({"work_st" : data[1][i].work_place.substr(0,2),
									  "work_loc" : data[1][i].work_place,
									 "work_loc_name" : data[1][i].work_place_name,
									 "home_st" : data[1][i].home_place.substr(0,2),
									 "home_loc" : data[1][i].home_place,
									 "home_loc_name" : data[1][i].home_place_name,
									 "jobs" : parseInt(data[1][i].goods),
									"miles_05": parseInt(data[1][i].miles_05),
									"miles_10": parseInt(data[1][i].miles_10),
									"miles_20": parseInt(data[1][i].miles_20),
									"miles_50": parseInt(data[1][i].miles_50),
									"miles_100": parseInt(data[1][i].miles_100),
									"miles_gt100": parseInt(data[1][i].miles_gt100),
									"median_distance":Math.round(parseFloat(data[1][i].median_distance),2)
									})
				}
		 }
		break;
		case 'trade' :
		 var barchart_title = geo_name + " Trade, Transportation,\nand Utilities Jobs, " + year;
		 for (i = 0; i < data[0].length; i++) {
			barchart_data.push({"work_in_home_in" : parseInt(data[0][i].work_in_home_in_trade),
							"work_in_home_out" : parseInt(data[0][i].work_in_home_out_trade),
							"work_out_home_in" : parseInt(data[0][i].work_out_home_in_trade)
			})
		 }
		 for (i = 0; i < data[1].length; i++) {
				if(geo == 'county'){
					sankey_data.push({"work_st" : data[1][i].work_cty.substr(0,2),
					                  "work_loc" : data[1][i].work_cty,
									 "work_loc_name" : data[1][i].work_cty_name,
									 "home_st" : data[1][i].home_cty.substr(0,2),
									 "home_loc" : data[1][i].home_cty,
									 "home_loc_name" : data[1][i].home_cty_name,
									 "jobs" : parseInt(data[1][i].trade),
									"miles_05": parseInt(data[1][i].miles_05),
									"miles_10": parseInt(data[1][i].miles_10),
									"miles_20": parseInt(data[1][i].miles_20),
									"miles_50": parseInt(data[1][i].miles_50),
									"miles_100": parseInt(data[1][i].miles_100),
									"miles_gt100": parseInt(data[1][i].miles_gt100),
									"median_distance":Math.round(parseFloat(data[1][i].median_distance),2)
									})
				} else {
					sankey_data.push({"work_st" : data[1][i].work_place.substr(0,2),
									  "work_loc" : data[1][i].work_place,
									 "work_loc_name" : data[1][i].work_place_name,
									 "home_st" : data[1][i].home_place.substr(0,2),
									 "home_loc" : data[1][i].home_place,
									 "home_loc_name" : data[1][i].home_place_name,
									 "jobs" : parseInt(data[1][i].trade),
									"miles_05": parseInt(data[1][i].miles_05),
									"miles_10": parseInt(data[1][i].miles_10),
									"miles_20": parseInt(data[1][i].miles_20),
									"miles_50": parseInt(data[1][i].miles_50),
									"miles_100": parseInt(data[1][i].miles_100),
									"miles_gt100": parseInt(data[1][i].miles_gt100),
									"median_distance":Math.round(parseFloat(data[1][i].median_distance),2)
									})
				}
		 }
		break;
		case 'services' :
		var barchart_title = geo_name + " Service Industry Jobs, " + year;
		 for (i = 0; i < data[0].length; i++) {
			barchart_data.push({"work_in_home_in" : parseInt(data[0][i].work_in_home_in_services),
							"work_in_home_out" : parseInt(data[0][i].work_in_home_out_services),
							"work_out_home_in" : parseInt(data[0][i].work_out_home_in_services)
			})
		 }
		 for (i = 0; i < data[1].length; i++) {
				if(geo == 'county'){
					sankey_data.push({"work_st" : data[1][i].work_cty.substr(0,2),
									  "work_loc" : data[1][i].work_cty,
									 "work_loc_name" : data[1][i].work_cty_name,
									 "home_st" : data[1][i].home_cty.substr(0,2),
									 "home_loc" : data[1][i].home_cty,
									 "home_loc_name" : data[1][i].home_cty_name,
									 "jobs" : parseInt(data[1][i].services),
									"miles_05": parseInt(data[1][i].miles_05),
									"miles_10": parseInt(data[1][i].miles_10),
									"miles_20": parseInt(data[1][i].miles_20),
									"miles_50": parseInt(data[1][i].miles_50),
									"miles_100": parseInt(data[1][i].miles_100),
									"miles_gt100": parseInt(data[1][i].miles_gt100),
									"median_distance":Math.round(parseFloat(data[1][i].median_distance),2)
									})
				} else {
					sankey_data.push({"work_st" : data[1][i].work_place.substr(0,2),
									  "work_loc" : data[1][i].work_place,
									 "work_loc_name" : data[1][i].work_place_name,
									 "home_st" : data[1][i].home_place.substr(0,2),
									 "home_loc" : data[1][i].home_place,
									 "home_loc_name" : data[1][i].home_place_name,
									 "jobs" : parseInt(data[1][i].services),
									"miles_05": parseInt(data[1][i].miles_05),
									"miles_10": parseInt(data[1][i].miles_10),
									"miles_20": parseInt(data[1][i].miles_20),
									"miles_50": parseInt(data[1][i].miles_50),
									"miles_100": parseInt(data[1][i].miles_100),
									"miles_gt100": parseInt(data[1][i].miles_gt100),
									"median_distance":Math.round(parseFloat(data[1][i].median_distance),2)
									})
				}
		 }
		break;
		} //Switch
		
//Create barchart Data
var labs= ["Employed and Live<br>in Selected Area",
		   "Live in Selected Area,<br>Employed Outside Area",
		   "Employed in Selected Area,<br>Live Outside Area"]

var vals = [barchart_data[0].work_in_home_in, barchart_data[0].work_out_home_in, barchart_data[0].work_in_home_out]
var fmtvals = [
 "Employed and Live in Selected Area " + fmt_comma(barchart_data[0].work_in_home_in),
 "Live in Selected Area, Employed Outside Area " + fmt_comma(barchart_data[0].work_out_home_in), 
 "Employed in Selected Area, Live Outside Area " + fmt_comma(barchart_data[0].work_in_home_out) 
]
var barcolor = ["#b2df8a", "#1f78b4", "#a6cee3"]

// Generate Sankey Data
 
//Summarizing out out of state and adjoining state residents
var out_state_home = sankey_data.filter(d =>  !['08','04','20','31','35','40','49','56'].includes(d.home_st))
var out_adjoining_home = sankey_data.filter(d => ['04','20','31','35','40','49','56'].includes(d.home_st))

var out_state_home_val = d3.rollup(out_state_home, v => d3.sum(v, d => d.jobs));

var out_state_home = [{"work_st" : "08",
					  "work_loc" : fips_code,
					 "work_loc_name" : geo_name,
					 "home_st" : "99",
					 "home_loc" : "99000",
					 "home_loc_name" : "Other, non-Adjoining States",
					 "jobs" : out_state_home_val
 }];

var out_state_work = sankey_data.filter(d => !['08','04','20','31','35','40','49','56'].includes(d.work_st))
var out_adjoining_work = sankey_data.filter(d => ['04','20','31','35','40','49','56'].includes(d.work_st))

var out_state_work_val = d3.rollup(out_state_work, v => d3.sum(v, d => d.jobs));

var out_state_work =[{"work_st" : "99",
					  "work_loc" : "99000",
					 "work_loc_name" : "Other, non-Adjoining States",
					 "home_st" : "08",
					 "home_loc" : fips_code,
					 "home_loc_name" : geo_name,
					 "jobs" : out_state_work_val
 }];

var in_state = sankey_data.filter(d => d.home_st == "08" & d.work_st == "08")
var out_work =  in_state.filter(d => d.work_loc == fips_code);
var out_home = in_state.filter(d => d.home_loc == fips_code);

var total_work = out_work.concat(out_adjoining_home,out_state_home).sort(function(a, b){ return d3.ascending(a['home_loc'], b['home_loc']); })
       .sort(function(a, b){ return d3.descending(a['jobs'], b['jobs']); });

var total_home = out_home.concat(out_adjoining_work, out_state_work).sort(function(a, b){ return d3.ascending(a['work_loc'], b['work_loc']); })
       .sort(function(a, b){ return d3.descending(a['jobs'], b['jobs']); });;
	   
	   

//Building Final Data Set

var sankey_fin = total_work.concat(total_home)

// Creating Nodeslist

var nodeslist_tmp = [];
var sankey_out = []  //sankey output data
sankey_fin.forEach(d => {
	nodeslist_tmp.push({
		residential_location : d.home_loc == fips_code ? geo_name : d.home_loc_name.replace(", CO",""),
		work_location : d.work_loc == fips_code ? geo_name : d.work_loc_name.replace(", CO",""),
		jobs : d.work_loc == fips_code ? d.jobs : d.jobs * -1,
		miles_05 : d.miles_05,
		miles_10 : d.miles_10,
		miles_20 : d.miles_20,
		miles_50 : d.miles_50,
		miles_100 : d.miles_100,
		miles_gt100 : d.miles_gt100,
		median_distance : d.median_distance

	})
	sankey_out.push({
		residential_location : d.home_loc == fips_code ? geo_name : d.home_loc_name.replace(", CO",""),
		work_location : d.work_loc == fips_code ? geo_name : d.work_loc_name.replace(", CO",""),
		jobs : d.jobs,
		miles_05 : d.miles_05,
		miles_10 : d.miles_10,
		miles_20 : d.miles_20,
		miles_50 : d.miles_50,
		miles_100 : d.miles_100,
		miles_gt100 : d.miles_gt100,
		median_distance : d.median_distance
	})
})


var nodeslist_tmp2 = supressData(nodeslist_tmp, fips_code, geo_name, 'lodes');

// Remove work in live in and summary records
var label_dat = [];
for(i = 0; i < nodeslist_tmp2.length ;i++){
	var res_str = nodeslist_tmp2[i].residential_location
	var work_str = nodeslist_tmp2[i].work_location
   if(res_str.indexOf("workers") != -1){
	   label_dat.push({
		   residential_location : nodeslist_tmp2[i].residential_location,
		   work_location : nodeslist_tmp2[i].work_location,
		   jobs : nodeslist_tmp2[i].jobs
	   })
   }
   if(work_str.indexOf("workers") != -1){
	   label_dat.push({
		   residential_location : nodeslist_tmp2[i].residential_location,
		   work_location : nodeslist_tmp2[i].work_location,
		   jobs : nodeslist_tmp2[i].jobs
	   })
   }
   if(nodeslist_tmp2[i].residential_location == nodeslist_tmp2[i].work_location){
	   label_dat.push({
		   residential_location : nodeslist_tmp2[i].residential_location,
		   work_location : nodeslist_tmp2[i].work_location,
		   jobs : nodeslist_tmp2[i].jobs
	   })
   }
}

label_dat = label_dat.filter((value, index, self) =>
  index === self.findIndex((t) => (
    t.residential_location === value.residential_location && t.work_location === value.work_location && t.jobs == value.jobs
  ))
)

var same_loc =[];
var annot_lab = [];
label_dat.forEach(d => {
	if(d.residential_location == d.work_location){
		same_loc.push({
			residential_location : d.residential_location,
			work_location : d.work_location,
		    jobs : d.jobs
			})
	}
	if(d.residential_location.includes('workers')){
		annot_lab.push({outlab: "In Commuters: " + d.residential_location})
	}
	if(d.work_location.includes('workers')){
		annot_lab.push({outlab : "Out Commuters: " + d.work_location})
	}
})

var nodeslist_tmp3 = nodeslist_tmp2.filter(d => !d.residential_location.includes("workers"))
                      .filter(d => !d.work_location.includes("workers")) 
                      .filter(d => d.residential_location != d.work_location)
					  
var nodeslist_tmp4 = [];
nodeslist_tmp3.forEach(d => {
	nodeslist_tmp4.push({work_location : d.work_location,
	                     residential_location : d.residential_location,
						jobs : d.jobs 
					})
})

var zero_node = []
zero_node.push({residential_location : geo_name,
				work_location : geo_name,
				jobs : 1
})

if(vals[0] == 0){
	var nodeslist_dat = zero_node.concat(same_loc,nodeslist_tmp4)
} else {
	var nodeslist_dat = same_loc.concat(nodeslist_tmp4)
}

var labarr_dat = [];
var lab1_tmp = []
var lab2_tmp = []
nodeslist_dat.forEach(obj => { 
	lab1_tmp.push(obj.residential_location)
	lab2_tmp.push(obj.work_location)
})

var lab1_dat = [...new Set(lab1_tmp)];
var lab2_dat = [...new Set(lab2_tmp)];
var labarr_dat = lab1_dat.concat(lab2_dat);


var neg = nodeslist_dat.filter(d => d.jobs <= 0).length
var pos = nodeslist_dat.filter(d => d.jobs > 0).length

if(neg < pos) {
	var inc = 1/pos;
} else {
	var inc = 1/neg;
}
var incr = parseFloat(inc.toFixed(3))

// Prepping nodelsist data

var total_pos_datmig = 0;
var total_neg_datmig = 0;
var y_dat_pos = 0.1;
var y_dat_neg = 0.1;
var titleVal_dat = "Commuting Profile "+ geo_name;
var citStr = "U.S. Census Bureau. LEHD Origin-Destination Employment Statistics (2002-2021) " +
	           " Print Date: "+ fmt_date(new Date);



//setting annotation labels
var annot_in = ""
var annot_out = ""
annot_lab.forEach(d => {
	if(d.outlab.includes("In Commuters")){
		annot_in = d.outlab
	}
	if(d.outlab.includes("Out Commuters")){
		annot_out = d.outlab
	}
})
var lab_annotation = [];

// Prepping nodeslist data
var tgt_neg = pos;

for(i = 0; i < nodeslist_dat.length;i++){
		if(nodeslist_dat[i].jobs < 0) {  //live in area work elsewhere
			nodeslist_dat[i].src = labarr_dat.indexOf(nodeslist_dat[i].residential_location)
			nodeslist_dat[i].tgt = tgt_neg
			nodeslist_dat[i].val = Math.abs(nodeslist_dat[i].jobs)
			nodeslist_dat[i].lablink = nodeslist_dat[i].residential_location + " to " + nodeslist_dat[i].work_location + ": " + fmt_comma(Math.abs(nodeslist_dat[i].jobs));	
			nodeslist_dat[i].xpos =  0.9;
			nodeslist_dat[i].labposx = 1;
			nodeslist_dat[i].ypos =  parseFloat(y_dat_neg.toFixed(3));
			nodeslist_dat[i].labposy = parseFloat(y_dat_neg.toFixed(3));
			nodeslist_dat[i].lab = nodeslist_dat[i].work_location;
			nodeslist_dat[i].linewidth = parseFloat((Math.abs(nodeslist_dat[i].jobs)/vals[2]).toFixed(2))
			y_dat_neg = y_dat_neg + incr;
			tgt_neg++
			
		} else {
			nodeslist_dat[i].src = labarr_dat.indexOf(nodeslist_dat[i].residential_location)
			nodeslist_dat[i].tgt = 0
			nodeslist_dat[i].val = Math.abs(nodeslist_dat[i].jobs)
			nodeslist_dat[i].lablink = nodeslist_dat[i].residential_location + " to " + nodeslist_dat[i].work_location + ": " + fmt_comma(Math.abs(nodeslist_dat[i].jobs));	
			nodeslist_dat[i].xpos =  0.1;
			nodeslist_dat[i].ypos =  parseFloat(y_dat_pos.toFixed(3));
			nodeslist_dat[i].labposx = 0;
			nodeslist_dat[i].labposy =  parseFloat(y_dat_pos.toFixed(3));
			nodeslist_dat[i].lab = nodeslist_dat[i].residential_location;
			nodeslist_dat[i].linewidth = parseFloat((Math.abs(nodeslist_dat[i].jobs)/vals[1]).toFixed(2))
			y_dat_pos = y_dat_pos + incr;
		}
		if(nodeslist_dat[i].residential_location == nodeslist_dat[i].work_location){
			nodeslist_dat[i].src = 0;
			nodeslist_dat[i].tgt = 0;
			nodeslist_dat[i].xpos =  0.5;
			nodeslist_dat[i].ypos =  0.5;
			nodeslist_dat[i].labposx = 0;
			nodeslist_dat[i].labposy =  0;
			nodeslist_dat[i].lab = nodeslist_dat[i].residential_location;
			nodeslist_dat[i].lablink = "Work and Live in " + nodeslist_dat[i].residential_location + ": "+ fmt_comma(Math.abs(nodeslist_dat[i].jobs))
			nodeslist_dat[i].linewidth = parseFloat((Math.abs(nodeslist_dat[i].jobs)/vals[0]).toFixed(2))
		}
		
		//Build Label Annotations
			lab_annotation.push({text: nodeslist_dat[i].residential_location,
				font : {size : 11, color : 'black'},    
				x : nodeslist_dat[i].xpos,
				y : nodeslist_dat[i].ypos,
				showarrow : false})
} //i

//Bar Chart Section
//Plotting 
var config = {responsive: true,
              displayModeBar: false};
			  


var bardata = [{
	 x: vals,
	 y: labs,
	 type: 'bar',
	 marker : {color : barcolor},
	 customdata : fmtvals,
	 hovertemplate : '%{customdata}',
	 hoverlabel : {namelength :0},
     orientation: 'h'
}]

var barlayout = {
	title: barchart_title,
  autosize: false,
  width: 1000,
  height: 500,
  margin: {
    l: 200,
    r: 50,
    b: 100,
    t: 100,
    pad: 4
  },
  xaxis : {
	title : 'Jobs'
  },
 	annotations : [{text :  citStr, 
      font: { size : 9, color: 'black'},
      xref : 'paper', 
	  yref : 'paper', 
	  xanchor : 'left',
	  yanchor : 'bottom',
      x : 0, 
      y : -0.25, 
      align : 'left', 
      showarrow : false}]
};


Plotly.newPlot(CHART0, bardata,barlayout,config);
var barchart_csv = document.getElementById('barchart_csv');
var barchart_png = document.getElementById('barchart_png');
barchart_csv.onclick = function() {exportToCsv(geo_name, 'LODESBAR', barchart_data,0)};
barchart_png.onclick = function() {exportToPng(geo_name, 'LODESBAR', CHART0,0)};

//Sankey Chart

var data_com = {
  type: "sankey",
  orientation: "h",
  arrangement : "fixed",
  node: {
    thickness: 50,
    line: {
      color: "black",
      width: nodeslist_dat.map(d => d.linewidth)
    },
   label: nodeslist_dat.map(d => d.lab),
   x : nodeslist_dat.map(d => d.xpos),
   y : nodeslist_dat.map(d => d.ypos),
   pad : 35,
   hoverinfo: 'none'
      },

  link: {
    source: nodeslist_dat.map(d => d.src),
    target: nodeslist_dat.map(d => d.tgt),
    value:  nodeslist_dat.map(d => d.val),
	customdata : nodeslist_dat.map(d => d.lablink),
	hovertemplate : '%{customdata}<extra></extra>'
  }
}

var data_commut = [data_com];

var layout_commut = {
  title: barchart_title + " Commuting Flows", 
  autosize : false, 
  width: 1000,
  height: 1000, 
  font: {
    size: 11,
	family : 'Arial Black'
  },
annotations : [
      {text :  citStr, 
      font: { size : 9, color: 'black'},
      xref : 'paper', 
	  yref : 'paper', 
	  xanchor : 'left',
	  yanchor : 'bottom',
      x : 0, 
      y : -0.10, 
      align : 'left', 
      showarrow : false},
		{text : annot_in,
        font : {size : 10, color : 'black'},      
		xref : 'paper', 
	    yref : 'paper', 
	    xanchor : 'left',
	    yanchor : 'bottom',
        x : 0,
        y : -0.09,
		showarrow : false },
		{text : annot_out,
        font : {size : 10, color : 'black'},      
		xref : 'paper', 
	    yref : 'paper', 
	    xanchor : 'left',
	    yanchor : 'bottom',
        x : 0.6,
        y : -0.09,
		showarrow : false }


]
}
Plotly.newPlot(CHART1, data_commut,layout_commut,config);
var sankey_csv = document.getElementById('sankey_csv');
var sankey_png = document.getElementById('sankey_png');
sankey_csv.onclick = function() {exportToCsv(geo_name, 'LODESFLOW', sankey_fin,0)};
sankey_png.onclick = function() {exportToPng(geo_name, 'LODESFLOW', CHART1,0)};

//Creating work tables

switch(sector){
		case 'total' :
			var work_title = "Work in "+ geo_name + " and Live elsewhere: All Jobs";
			var fileName_work = "Commuting data Work in "+ geo_name +", Live elsewhere All Jobs"
			var res_title = "Live in "+ geo_name + " and Work elsewhere: All Jobs";
			var fileName_res = "Commuting data Live in "+ geo_name +", Work elsewhere All Jobs"
			break;
		case 'goods' :
			var work_title = "Work in "+ geo_name + " and Live elsewhere: Goods Producing Jobs";
			var fileName_work = "Commuting data Work in "+ geo_name +", Live elsewhere Goods Producing Jobs"
			var res_title = "Live in "+ geo_name + " and Work elsewhere: Goods Producing Jobs";
			var fileName_res = "Commuting data Live in "+ geo_name +", Work elsewhere Goods Producing Jobs"
			break;
		case 'trade' :
			var work_title = "Work in "+ geo_name + " and Live elsewhere: Trade, Transportation, and Utilities Jobs";
			var fileName_work = "Commuting data Work in "+ geo_name +", Live elsewhere Trade Jobs"
			var res_title = "Live in "+ geo_name + " and Work elsewhere: Trade, Transportation, and Utilities Jobs";
			var fileName_res = "Commuting data Live in "+ geo_name +", Work elsewhere Trade Jobs"
			break;
		case 'services' :
			var work_title = "Work in "+ geo_name + " and Live elsewhere: Service Industry Jobs";
			var fileName_work = "Commuting data Work in "+ geo_name +", Live elsewhere Service Jobs"
			var res_title = "Live in "+ geo_name + " and Work elsewhere: Service Industry Jobs";
			var fileName_res = "Commuting data Live in "+ geo_name +", Work elsewhere Service Jobs"
			break;
	}
var work_tab = nodeslist_tmp3.filter(d => d.work_location == geo_name)

var work_tab_out = lodes_tab(work_tab,citStr,"work")
var work_tab_html = work_tab_out[0]
var work_tab_html2 = "<h2>" + work_title + "</h2><p></p><table>"+ work_tab_html + "</table>"
var labels_work = work_tab_out[1]
var footArr = work_tab_out[2]

//Clear div
var pgLength_work = 10;
var tabName0 = "work_tab_out"
TAB0.innerHTML = "";

var tabObj_work = "#" + tabName0;
$(TAB0).append("<h2>"+work_title+"</h2>")
$(TAB0).append("<table id= '"+ tabName0 + "' class='DTTable' width='90%'></table>");
$(tabObj_work).append(work_tab_html); //this has to be a html table

$(tabObj_work).DataTable({
       "pageLength" : pgLength_work,
	   "ordering": false,
		"fixedHeader":   true ,
 dom: 'Bfrtip',
       buttons: [
		{ text : 'Word', action: function ( e, dt, node, config ) { export2Word(work_tab_html2,fileName_work)} },
		{ text : 'CSV', action: function ( e, dt, node, config ) { exportToCsv(geo_name, 'LODESFLOW', sankey_fin,0)} },
		]  //buttons
 } );
 
//Creating Residential tables

var res_tab = nodeslist_tmp3.filter(d => d.residential_location == geo_name)

var res_tab_out = lodes_tab(res_tab,citStr,"res")
var res_tab_html = res_tab_out[0]
var res_tab_html2 = "<h2>" + res_title + "</h2><p></p><table>"+ res_tab_html + "</table>"
var labels_res = res_tab_out[1]
var footArr = res_tab_out[2]


//Clear div
var pgLength = 10;
var tabName1 = "res_tab_out"
TAB1.innerHTML = "";

var tabObj_res = "#" + tabName1;
$(TAB1).append("<h2>"+res_title+"</h2>")
$(TAB1).append("<table id= '"+ tabName1 + "' class='DTTable' width='90%'></table>");
$(tabObj_res).append(res_tab_html); //this has to be a html table

$(tabObj_res).DataTable({
       "pageLength" : pgLength,
	   "ordering": false,
		"fixedHeader":   true ,
 dom: 'Bfrtip',
       buttons: [
		{ text : 'Word', action: function ( e, dt, node, config ) { export2Word(res_tab_html2,fileName_res)} },
		{ text : 'CSV', action: function ( e, dt, node, config ) { exportToCsv(geo_name, 'LODESFLOW', sankey_fin,0)} }
		
		]  //buttons
 } );
}) //Promise
}
//genLODES