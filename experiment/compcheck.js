/* set up comprehension check on instructions slide */
	
// set up button behavior
$('.slide#instructions button').click(function() {
	
	// record data
	experiment.allData.comprehensionCheck = $('textarea#comprehensionCheck').val();

	// set parameters of this session
	experiment.trials = swatches.slice();
	experiment.condition = chosenCondition.condition.slice();

	// go to first trial
	experiment.next();
});