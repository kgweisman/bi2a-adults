// get date
var date = new Date();

// set up condition variable (random assignment)
var chosenCondition = randomElementNR(conditions);

// create experiment object
var experiment = {
	trials: [],
	// questionTypes: ["do you think this one can think?", "do you think this one has feelings?", "do you think this one can sense things nearby?", "do you think this can feel happy?", "do you think this one can feel hungry?", "do you think this one can feel pain?"],
	allData: {
		// fingerprinting information
		fingerprintData: {},

		// condition and session information
		condition: chosenCondition.condition,

		// demographic information about participant
		age: "",
		gender: "",
		education: "",
		ethnicity: [],
		religion: [],
		englishNative: "",
		comments: "",

		// trial by trial data
		trialData: [],		
	},

	// what happens after completing all trials
	end: function() {

		// show demographics survey	
		showSlide("demographics");
	},

	// what happens when participant sees a new trial
	next: function() {
		if (this.trials.length === 0) {

			// move on to end of experiment
			experiment.end();

		} else {

			// create place to store data for this trial
			var data = {
				trialNum: 49 - this.trials.length,
				swatch: "",
				response: "",
				responseCoded: NaN,
				rt: NaN
			};

			// display progress bar
			var percentComplete = (data.trialNum-1)/49 * 100;
			var percentCompleteRounded = Math.round(percentComplete);
			$('#stage .progress-text').text("trial "+data.trialNum.toString()+" of 48: "+percentCompleteRounded+"% complete");
			$('#stage .progress-bar').attr("aria-valuenow", percentComplete.toString());
			$('#stage .progress-bar').css("width", percentComplete.toString()+"%");

			// display question
			$('.slide#stage span#question-text').text(chosenCondition.question);

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
				experiment.allData.trialData.push(data);
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

