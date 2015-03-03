// get date
var date = new Date();

// set up condition variable (random assignment)
// var chosenCondition = randomElementNR(swatchSets);
var chosenCondition = randomElementNR(["animalCategorization", "propertyInference"]);

// create experiment object
var experiment = {
	trials: [],
	// bonusTrials: [],
	practiceTrials: [],
	questionTypes: ["do you think this one can think?", "do you think this one has feelings?", "do you think this one can sense things nearby?", "do you think this can feel happy?", "do you think this one can feel hungry?", "do you think this one can feel pain?"],
	condition: chosenCondition,
	trialData: [],

	// what happens after completing all trials
	end: function() {

		// show ending slide	
		showSlide("finished");
		
		$('.slide#finished button').click(function() { 
			// reload html to return to start slide
			location.reload();
		});

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
		    	condition: "condition"
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

	// // what happens when participant plays bonus rounds
	// bonus: function() {

	// 	// set up how to play a bonus trial
	// 	function playBonus() {

	// 		// create place to store data for this bonus trial
	// 		var data = {
	// 			phase: "bonus",
	// 			question: "",
	// 			trialNum: 19 - experiment.bonusTrials.length,
	// 			swatch: "",
	// 			response: "",
	// 			responseCoded: "",
	// 			rt: NaN
	// 		}

	// 		// display progress bar
	// 		var percentComplete = (data.trialNum-13)/6 * 100;
	// 		$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
	// 		$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

	// 		// restyle progress bar
	// 		$('#stage .progress').css("background-color", "rgba(170, 120, 240, .1)");
	// 		$('#stage .progress-bar').css("background-color", "rgba(170, 120, 240, 1)");

	// 		// choose random image to display
	// 		var chosenSwatch = randomElementNR(experiment.bonusTrials);
	// 		data.swatch = chosenSwatch.swatchName;

	// 		// display chosen image
	// 		$('.slide#stage img').attr("src", chosenSwatch.imageSource);

	// 		// display randomly selected question
	// 		var chosenQuestion = randomElementNR(experiment.questionTypes);
	// 		data.question = chosenQuestion.slice();
	// 		$('.slide#stage #question').text(chosenQuestion);

	// 		// show trial
	// 		showSlide("stage");

	// 		// record response and rt
	// 		var startTime = (new Date()).getTime();

	// 		var clickHandler = function(event) {
	// 			var endTime = (new Date()).getTime();
	// 			data.rt = endTime - startTime;
	// 			experiment.trialData.push(data);
	// 		};

	// 		$('.slide#stage button[type="submit"]').click(function() {
	// 			// record response
	// 			data.response = $(this).attr('id');
	// 			data.responseCoded = parseFloat($(this).attr('value'));

	// 			// end trial
	// 			clickHandler();
	// 			$('.slide#stage button[type="submit"]').unbind().blur();
	// 			window.scrollTo(0, 0);
	// 			experiment.next();
	// 		});
	// 	};

	// 	if (experiment.bonusTrials.length === 0) {

	// 		// end study session
	// 		experiment.end();

	// 	} else if (experiment.bonusTrials.length === 6) {

	// 		$(".slide").hide();

	// 		// give option of bonus round
	// 		var chooseBonus = window.confirm("Do you want to play a bonus round?");
	// 		if (chooseBonus === true) {
	// 			// if child says yes...
	// 			playBonus();

	// 		} else {
	// 		// if child says no...

	// 		experiment.end();
	// 		}			

	// 	} else {
	// 		playBonus();
	// 	}
	// },

	// what happens when participant does practice trials
	practice: function() {

		// set up how to play a practice trial
		function playPractice() {

			// create place to store data for this practice trial
			var data = {
				phase: "practice",
				question: "do you think this one is an animal?",
				trialNum: 3 - experiment.practiceTrials.length,
				swatch: "",
				response: "",
				responseCoded: "",
				rt: NaN
			}

			// display progress bar
			var percentComplete = (data.trialNum-1)/51 * 100;
			$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

			// choose random image to display
			var chosenSwatch = randomElementNR(experiment.practiceTrials);
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
				experiment.practice();
			});
		};

		if (experiment.practiceTrials.length === 0) {

			// advance to real trials
			experiment.next();

		} else {

			// do practice trials
			playPractice();
		}
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
				question: "do you think this one is an animal?",
				trialNum: 51 - this.trials.length,
				swatch: "",
				response: "",
				responseCoded: NaN,
				rt: NaN
			};

			// display progress bar
			var percentComplete = (data.trialNum-1)/51 * 100;
			$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

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
	experiment.practiceTrials = swatchSetPractice.slice();
	experiment.trials = swatches.slice();
	// experiment.bonusTrials = [swatches[0], swatches[1], swatches[5], swatches[6], swatches[10], swatches[11]];
	experiment.condition = chosenCondition.slice();

	// go to practice trial
	experiment.practice();
});

// // bail out if needed
// $('.slide#stage button[type="end"]').click(function() { 
// 	// go to end
// 	experiment.end();
// });

// start!
showSlide("preload");

