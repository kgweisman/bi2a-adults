// get date
var date = new Date();

// set up condition variable (random assignment)
var chosenCondition = randomElementNR(conditions);

// create experiment object
var experiment = {
	trials: [],
	// questionTypes: ["do you think this one can think?", "do you think this one has feelings?", "do you think this one can sense things nearby?", "do you think this can feel happy?", "do you think this one can feel hungry?", "do you think this one can feel pain?"],
	condition: chosenCondition.condition,
	questions: chosenCondition.questions.slice(),
	trialData: [],

	// what happens after completing all trials
	end: function() {

		// show ending slide	
		showSlide("finished");
		
		// export data to csv
		var data = experiment.trialData;
 
		function DownloadJSON2CSV(objArray) { // code source: http://www.zachhunter.com/2010/11/download-json-to-csv-using-javascript/
		    // get trial-level info
		    var array = typeof objArray != 'object' ? JSON.parse(objArray) : objArray;

		    // add subject-level info
		    for (trial in objArray) {
		    	objArray[trial].condition = experiment.condition;
		    };

		    // add headers in a hacky way
		    objArray.unshift({
		    	phase: "phase",
		    	question: "question",
		    	trialNum: "trialNum",
		    	swatch: "swatch",
		    	response: "response",
		    	responseCoded: "responseCoded",
		    	rt: "rt",
		    	condition: "condition",
		    });

		    // convert to csv
		    var str = '';
		     
		    for (var i = 0; i < array.length; i++) {
		        var line = '';
		        for (var index in array[i]) {
		            if(line != '') line += ','
		         
		            line += array[i][index];
		        }
		 
		        str += line + '\r\n';
		    }
		 
		    if (navigator.appName != 'Microsoft Internet Explorer')
		    {
		        window.open('data:text/csv;charset=utf-8,' + escape(str));
		    }
		    else
		    {
		        var popup = window.open('','csv','');
		        popup.document.body.innerHTML = '<pre>' + str + '</pre>';
		    }          
		}
		DownloadJSON2CSV(data);
	},

	// what happens when participant sees a new trial
	next: function() {
		if (this.trials.length === 0) {

			// move on to end of experiment
			experiment.end();

		} else {

			// create place to store data for this trial
			var data = {
				phase: "study",
				trialNum: 49 - this.trials.length,
				swatch: "",
				question1: "",
				response1: "",
				responseCoded1: NaN,
				question2: "",
				response2: "",
				responseCoded2: NaN,
				question3: "",
				response3: "",
				responseCoded3: NaN,
				question4: "",
				response4: "",
				responseCoded4: NaN,
				question5: "",
				response5: "",
				responseCoded5: NaN,
				question6: "",
				response6: "",
				responseCoded6: NaN,
				rt: NaN
			};

			// display progress bar
			var percentComplete = (data.trialNum-1)/49 * 100;
			$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

			// choose random order of questions
			var questionsOrder = chosenCondition.questions.slice()

			for (i = 0; i < chosenCondition.questions.length; i++) {
				// var question = (question+1).toString();
				data["question"+(i+1)] = randomElementNR(questionsOrder);
			}

			// display questions
			if (experiment.condition === "animalCategorization") {
				console.log("yes");
				$('.slide#stage span#question1').text(data.question1);
				$('div#question2.control-group').hide();
				$('div#question3.control-group').hide();
				$('div#question4.control-group').hide();
				$('div#question5.control-group').hide();
				$('div#question6.control-group').hide();
			} else {
				$('.slide#stage span#question1').text(data.question1);
				$('.slide#stage span#question2').text(data.question2);
				$('.slide#stage span#question3').text(data.question3);
				$('.slide#stage span#question4').text(data.question4);
				$('.slide#stage span#question5').text(data.question5);
				$('.slide#stage span#question6').text(data.question6);
			}

			// choose random image to display
			var chosenSwatch = randomElementNR(this.trials);
			data.swatch = chosenSwatch.swatchName;

			// display chosen image
			$('.slide#stage img').attr("src", chosenSwatch.imageSource);

			// show trial
			showSlide("stage");

			// record response and rt
			var startTime = (new Date()).getTime();

			var clickHandler = function(event) {
				var endTime = (new Date()).getTime();
				data.rt = endTime - startTime;
				experiment.trialData.push(data);
			};

			$('.slide#stage button[type="submit"]').click(function() {
				// record response
				data.response = $(this).attr('id');
				data.responseCoded = parseFloat($(this).attr('value'));

				// end trial
				clickHandler();
				$('.slide#stage button[type="submit"]').unbind().blur();
				window.scrollTo(0, 0);
				experiment.next();
			});
		}
	}
};

// set up button behavior
$('.slide#consent button').click(function() { 
	showSlide("instructions");
});

// continue to experiment
$('.slide#instructions button').click(function() { 

	// set parameters of this session
	experiment.trials = swatches.slice();
	experiment.condition = chosenCondition.condition.slice();

	// go to first trial
	experiment.next();
});

// start!
showSlide("preload");

