/* set up comprehension check on instructions slide */
	
// display question
$('.slide#instructions span#question-text').text(chosenCondition.question);

// set up button behavior
$('.slide#instructions button').click(function() {

	if ($('textarea#comprehensionCheck').val() == "") {
	window.alert("Please restate the sentence before continuing.");
	} else if ($('textarea#comprehensionCheck').val() == "I think that this one "+chosenCondition.question) {
	window.alert("Please restate the sentence in your own words.");
	} else if ($('textarea#comprehensionCheck').val() == "I think that this one "+chosenCondition.question+".") {
	window.alert("Please restate the sentence in your own words.");
	} else {
		// record data
		experiment.allData.comprehensionCheck = $('textarea#comprehensionCheck').val();

		// set parameters of this session
		experiment.trials = swatches.slice();
		experiment.condition = chosenCondition.condition.slice();

		// go to first trial
		experiment.next();
	}

});