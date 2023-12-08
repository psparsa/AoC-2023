const input = `seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4`;

const [
  seedsChunk,
  seedToSoilChunk,
  soilToFertilizerChunk,
  fertilizerToWaterChunk,
  waterToLightChunk,
  lightToTemperatureChunk,
  temperatureToHumidityChunk,
  humidityToLocationChunk,
] = input.split("\n\n");

const seedsNeedToPlant = (() => {
  const lines = seedsChunk.split("\n");
  return [lines[0].split(":")[1].trim(), ...lines.slice(1)].map((x) =>
    x.split(" ").map(Number)
  );
})();

const parseMap = (mapChunk: string) => {
  const formatStringToArray = (str: string) => str.split(" ").map(Number);
  return mapChunk.split("\n").slice(1).map(formatStringToArray);
};

const isInRange = (number: number) => (point1: number) => (point2: number) => {
  const min = Math.min(point1, point2);
  const max = Math.max(point1, point2);

  return number >= min && number <= max;
};

const seedToSoilMap = parseMap(seedToSoilChunk);
const soilToFertilizerMap = parseMap(soilToFertilizerChunk);
const fertilizerToWaterMap = parseMap(fertilizerToWaterChunk);
const waterToLightMap = parseMap(waterToLightChunk);
const lightToTemperatureMap = parseMap(lightToTemperatureChunk);
const temperatureToHumidityMap = parseMap(temperatureToHumidityChunk);
const humidityToLocationMap = parseMap(humidityToLocationChunk);

const mapToX = (source: number) => (map: number[][]) => {
  for (let i = 0; i < map.length; i++) {
    const [startDest, startSource, length] = map[i];
    if (isInRange(source)(startSource)(startSource + length - 1)) {
      const leftPosition = Math.abs(startSource - source);
      return startDest + leftPosition;
    }
  }

  return source;
};

const mapSeedToLocation = (seed: number) => {
  const a = mapToX(seed)(seedToSoilMap);
  const b = mapToX(a)(soilToFertilizerMap);
  const c = mapToX(b)(fertilizerToWaterMap);
  const d = mapToX(c)(waterToLightMap);
  const e = mapToX(d)(lightToTemperatureMap);
  const f = mapToX(e)(temperatureToHumidityMap);
  const g = mapToX(f)(humidityToLocationMap);

  return g;
};

const answer1 = Math.min(
  ...seedsNeedToPlant.flatMap((set) => {
    const tmp = set.map((n) => mapSeedToLocation(n));

    return tmp;
  })
);

const answer2 = (() => {
  const seeds = seedsNeedToPlant.flat(1);

  const pairs = Array.from({ length: seeds.length / 2 }).map((_, index) => {
    return [seeds[index * 2], seeds[index * 2 + 1]];
  });

  console.log(pairs);

  let store: number[] = [];

  for (let i = 0; i < pairs.length; i++) {
    const [start, length] = pairs[i];

    for (let j = start; j < start + length; j++) {
      const currentSeed = j;
      store.push(mapSeedToLocation(currentSeed));
    }
  }

  return Math.min(...store);
})();

// Answer: 35
console.log(answer1);

// Answer: 46
console.log(answer2);
