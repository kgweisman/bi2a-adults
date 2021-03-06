// compile swatches with image sources and adult ratings

function addSwatch(swatchName) {
	function Swatch(swatchName) {
		this.swatchName = swatchName;
		this.imageSource = "images/"+swatchName+".png";
	};
	newSwatch = new Swatch(swatchName);
	swatches.push(newSwatch);
}

var swatchNameList = [];
for (i = 0; i < 9; i++) {
	swatchNameList[i] = "BI2A0000"+(i+1);
}
for (i = 9; i < 48; i++) {
	swatchNameList[i] = "BI2A000"+(i+1);
}

var animalRatingAdultUSList = [6.300000, 6.466667, 6.600000, 6.166667, 6.666667, 6.666667, 6.500000, 6.400000, 6.500000, 6.266667, 6.700000, 4.966667, 5.433333, 5.433333, 6.100000, 4.600000, 2.611111, 3.500000, 5.800000, 2.900000, 1.900000, 3.633333, 2.666667, 6.033333, 1.533333, 4.785714, 1.800000, 3.866667, 1.166667, 1.733333, 1.066667, 3.066667, 1.233333, 2.433333, 1.066667, 1.166667, 1.766667, 1.466667, 1.066667, 1.066667, 1.133333, 1.700000, 1.933333, 1.033333, 1.033333, 1.033333, 1.133333, 1.100000];

var swatches = [];
for (i in swatchNameList) {
	addSwatch(swatchNameList[i]);
	swatches[i].animalRatingAdultUS = animalRatingAdultUSList[i];
}