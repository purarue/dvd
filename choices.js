function shuffle(array) {
  var currentIndex = array.length,
    temporaryValue,
    randomIndex;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {
    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}

let choices = shuffle([
  "DVD",
  "HDTV",
  "PVC",
  "HTC",
  "LSD",
  "ABC",
  "XYZ",
  "123",
  "PSP",
  "KFC",
  "SSD",
  "HDD",
  "THC",
  "PS3",
  "MP3",
  "AMD",
  "BRB",
  "MTV",
  "RPG",
  "LCD",
  "TNT",
  "USB",
  "USD",
  "SUV",
  "TTV",
  "DLC",
  "PVP",
  "NYC",
  "DDR3",
  "CRT",
  "PNG",
  "DLC",
  "RNG",
  "4KTV",
  "NPC",
  "MLG",
  "PHD",
]);
