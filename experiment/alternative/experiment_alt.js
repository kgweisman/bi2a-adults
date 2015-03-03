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
	trialQuestions: [],
	trialSwatch: "",
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
		    	questionNum: "questionNum",
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
			// track questions asked per trial
			experiment.trialQuestions = chosenCondition.questions.slice();
			console.log(experiment.trialQuestions);

			// choose random image and display
			experiment.chosenSwatch = randomElementNR(experiment.trials);
			$('.slide#stage img').attr("src", experiment.chosenSwatch.imageSource);

			// proceed through questions
			experiment.nextQuestion();
		}
	},

	// what happens when participant sees a new question for the same trial
	nextQuestion: function() {

		if (experiment.trialQuestions.length === 0) {
			experiment.next();

		} else {

			// create place to store data for this subtrial
			var data = {
				phase: "study",
				trialNum: 49 - this.trials.length,
				questionNum: (chosenCondition.questions.length + 1) - experiment.trialQuestions.length,
				swatch: "",
				question: "",
				response: "",
				responseCoded: NaN,
				rt: NaN
			};

			// store chosen image
			data.swatch = experiment.chosenSwatch.swatchName;

			// select question for subtrial and fill in question text
			data.question = randomElementNR(experiment.trialQuestions);
			$('.slide#stage span#question1').text(data.question);
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
				experiment.nextQuestion();
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
