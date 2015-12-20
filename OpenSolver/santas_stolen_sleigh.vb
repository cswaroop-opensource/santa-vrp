/* santas_stolen_sleigh.lsp */

use io;
use string;
use math;

function haversine(lat1, lon1, lat2, lon2) {
  // AVG_EARTH_RADIUS = 6371;
  local AVG_EARTH_RADIUS2 = 12742;
  lat = lat2 - lat1;
  lon = lon2 - lon1;
  d = math.pow(math.sin(lat / 2.0),2) + math.cos(lat1) * math.cos(lat2) * math.pow(math.sin(lon / 2),2);
  h = AVG_EARTH_RADIUS2 * math.asin(math.sqrt(d));
  return h;
}

function input() {
  local inFile = io.openRead(inFileName);
  inFile.readln();

  // north_pole
  position_id[0] = 0;
  position_lat[0] = 90 / 180.0 * math.pi;
  position_lon[0] = 0.0/ 180.0 * math.pi;
  position_weight[0] = 0.0;

  pos = 1;
  while(inFile.eof() == false) {
    local line = inFile.readln();
    local tokens = line.split(",");
    local id = tokens[0];
    local lat = tokens[1].toDouble() / 180.0 * math.pi;
    local lon = tokens[2].toDouble() / 180.0 * math.pi;
    local weight = tokens[3].toDouble();

    position_id[pos] = id;
    position_lat[pos] = lat;
    position_lon[pos] = lon;
    position_weight[pos] = weight;
    pos = pos + 1;
  }
	
  TRIP_BND = 100;
  NB_GIFTS = position_id.count() - 1;
  NB_TRIPS = max(1, min(2000, ceil(NB_GIFTS/10)));

  BASE_WEIGHT = 10;
  MAX_WEIGHT = 1010;
}

function param() {
  if(lsNbThreads == nil) lsNbThreads = 1;
  if(lsTimeLimit == nil) lsTimeLimit = 60;
}

function model() {
  trip[1..NB_TRIPS] <- list(NB_GIFTS);
  
  constraint partition[i in 1..NB_TRIPS](trip[i]);
  for[i in 1..NB_TRIPS]
    constraint count(trip[i]) <= min(TRIP_BND, NB_GIFTS);

  // weight constraints
  for[i in 1..NB_TRIPS] {
    total_weight[i] <- BASE_WEIGHT + sum[j in 1..min(TRIP_BND, NB_GIFTS)](position_weight[1+trip[i][j-1]]);
    constraint total_weight[i] <= MAX_WEIGHT;
  }
	
	// cumulative weight expression
  for[i in 1..NB_TRIPS] {
    remaining_weight[i][0] <- total_weight[i];
    for[j in 1..min(TRIP_BND, NB_GIFTS)-1]
      remaining_weight[i][j] <- remaining_weight[i][j-1] - position_weight[1+trip[i][j-1]];
  }
	
	// distance
  for[i in 1..NB_TRIPS] {
    distance[i] <- call(haversine, position_lat[0], position_lon[0], position_lat[1+trip[i][0]], position_lon[1+trip[i][0]]) * total_weight[i]
      + sum[j in 1..(min(TRIP_BND, NB_GIFTS)-1)](call(haversine, position_lat[1+trip[i][j-1]], position_lon[1+trip[i][j-1]], position_lat[1+trip[i][j]], position_lon[1+trip[i][j]]) * remaining_weight[i][j])
      + call(haversine, position_lat[1+trip[i][min(TRIP_BND, NB_GIFTS)-1]], position_lon[1+trip[i][min(TRIP_BND, NB_GIFTS)-1]], position_lat[0], position_lon[0]) * BASE_WEIGHT;
  }
	
  minimize sum[i in 1..NB_TRIPS](distance[i]);
}

function output() {
  if(solFileName == nil) solFile = io.openWrite("solution.csv");
  else solFile = io.openWrite(solFileName);
  solFile.println("GiftId,TripId");
  trip_id = 0;
  for[i in 1..NB_TRIPS] {
    current_trip = trip[i].value;
    if(current_trip.count() > 0) {
      for[position in current_trip] 
        solFile.println(position_id[1+position], ",", trip_id);
      trip_id += 1;
    }	
  }
  solFile.close();
}
