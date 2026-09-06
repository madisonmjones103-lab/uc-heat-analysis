"use strict";


////////////////////////////////////
// CAMPUS CONFIGURATION
////////////////////////////////////

const campusConfig = {

  ucla: {
    name: "UCLA",
    heatImage: "data/ucla_heat.png",
    ndviImage: "data/ucla_ndvi.png",
    metadataFile: "data/ucla_map_metadata.json",
    dataFile: "data/ucla_temperature_ndvi.csv"
  },

  berkeley: {
    name: "UC Berkeley",
    heatImage: "data/berkeley_heat.png",
    ndviImage: "data/berkeley_ndvi.png",
    metadataFile: "data/berkeley_map_metadata.json",
    dataFile: "data/berkeley_temperature_ndvi.csv"
  },

  ucd: {
    name: "UC Davis",
    heatImage: "data/ucd_heat.png",
    ndviImage: "data/ucd_ndvi.png",
    metadataFile: "data/ucd_map_metadata.json",
    dataFile: "data/ucd_temperature_ndvi.csv"
  },

  uci: {
    name: "UC Irvine",
    heatImage: "data/uci_heat.png",
    ndviImage: "data/uci_ndvi.png",
    metadataFile: "data/uci_map_metadata.json",
    dataFile: "data/uci_temperature_ndvi.csv"
  },

  ucm: {
    name: "UC Merced",
    heatImage: "data/ucm_heat.png",
    ndviImage: "data/ucm_ndvi.png",
    metadataFile: "data/ucm_map_metadata.json",
    dataFile: "data/ucm_temperature_ndvi.csv"
  },

  ucr: {
    name: "UC Riverside",
    heatImage: "data/ucr_heat.png",
    ndviImage: "data/ucr_ndvi.png",
    metadataFile: "data/ucr_map_metadata.json",
    dataFile: "data/ucr_temperature_ndvi.csv"
  },

  ucsd: {
    name: "UC San Diego",
    heatImage: "data/ucsd_heat.png",
    ndviImage: "data/ucsd_ndvi.png",
    metadataFile: "data/ucsd_map_metadata.json",
    dataFile: "data/ucsd_temperature_ndvi.csv"
  },

  ucsb: {
    name: "UC Santa Barbara",
    heatImage: "data/ucsb_heat.png",
    ndviImage: "data/ucsb_ndvi.png",
    metadataFile: "data/ucsb_map_metadata.json",
    dataFile: "data/ucsb_temperature_ndvi.csv"
  },

  ucsc: {
    name: "UC Santa Cruz",
    heatImage: "data/ucsc_heat.png",
    ndviImage: "data/ucsc_ndvi.png",
    metadataFile: "data/ucsc_map_metadata.json",
    dataFile: "data/ucsc_temperature_ndvi.csv"
  }

};


////////////////////////////////////
// APPLICATION STATE
////////////////////////////////////

let currentMode = "heat";

/*
  UCLA is now the default campus.
*/
let selectedCampusKey = "ucla";

let campusMap = null;

let campusMetadata = null;

let campusPixelData = [];

let campusHeatLayer = null;

let campusNdviLayer = null;

let campusChangeRequest = 0;


////////////////////////////////////
// HTML ELEMENTS
////////////////////////////////////

const heatButton =
  document.getElementById("heat-button");

const ndviButton =
  document.getElementById("ndvi-button");

const compareButton =
  document.getElementById("compare-button");

const campusSelect =
  document.getElementById("campus-select");

const campusMapTitle =
  document.getElementById("campus-map-title");

const mapPanelDescription =
  document.getElementById("map-panel-description");

const legendTitle =
  document.getElementById("legend-title");

const legendGradient =
  document.getElementById("legend-gradient");

const legendMin =
  document.getElementById("legend-min");

const legendMax =
  document.getElementById("legend-max");


const modeButtons = [
  heatButton,
  ndviButton,
  compareButton
];


////////////////////////////////////
// CHECK REQUIRED HTML
////////////////////////////////////

function validateHtmlElements() {

  const requiredElements = {

    heatButton,
    ndviButton,
    compareButton,

    campusSelect,

    campusMapTitle,
    mapPanelDescription,

    legendTitle,
    legendGradient,
    legendMin,
    legendMax

  };


  const missingElements = Object
    .entries(requiredElements)
    .filter(([, element]) => !element)
    .map(([name]) => name);


  if (missingElements.length > 0) {

    throw new Error(
      `Missing HTML elements: ${
        missingElements.join(", ")
      }`
    );

  }


  if (!document.getElementById("campus-map")) {

    throw new Error(
      'Missing HTML element with id="campus-map".'
    );

  }


  if (typeof Papa === "undefined") {

    throw new Error(
      "Papa Parse did not load."
    );

  }

}


////////////////////////////////////
// FETCH JSON
////////////////////////////////////

async function loadJson(filePath) {

  const response =
    await fetch(filePath);


  if (!response.ok) {

    throw new Error(
      `Could not load ${filePath}. ` +
      `HTTP status: ${response.status}`
    );

  }


  return response.json();

}


////////////////////////////////////
// FETCH AND PARSE CSV
////////////////////////////////////

async function loadCampusPixelData(filePath) {

  const response =
    await fetch(filePath);


  if (!response.ok) {

    throw new Error(
      `Could not load ${filePath}. ` +
      `HTTP status: ${response.status}`
    );

  }


  const csvText =
    await response.text();


  const parsed =
    Papa.parse(
      csvText,
      {
        header: true,
        dynamicTyping: true,
        skipEmptyLines: true
      }
    );


  if (parsed.errors.length > 0) {

    console.warn(
      `CSV warnings for ${filePath}:`,
      parsed.errors
    );

  }


  return parsed.data

    .map((row) => ({

      longitude:
        Number(row.longitude),

      latitude:
        Number(row.latitude),

      temperature:
        Number(row.temperature),

      ndvi:
        Number(row.ndvi),

      validObservations:
        Number(row.valid_observations)

    }))

    .filter((row) => (

      Number.isFinite(row.longitude) &&

      Number.isFinite(row.latitude)

    ));

}


////////////////////////////////////
// PRELOAD IMAGE
////////////////////////////////////

function preloadImage(filePath) {

  return new Promise(
    (resolve, reject) => {

      const image =
        new Image();


      image.onload = () => {

        resolve(filePath);

      };


      image.onerror = () => {

        reject(
          new Error(
            `Could not load image: ${filePath}`
          )
        );

      };


      image.src = filePath;

    }
  );

}


////////////////////////////////////
// READ METADATA BOUNDS
////////////////////////////////////

function getImageBounds(metadata) {

  if (
    !metadata ||
    !metadata.bounds
  ) {

    throw new Error(
      "Metadata does not contain bounds."
    );

  }


  const south =
    Number(metadata.bounds.south);

  const west =
    Number(metadata.bounds.west);

  const north =
    Number(metadata.bounds.north);

  const east =
    Number(metadata.bounds.east);


  const values = [
    south,
    west,
    north,
    east
  ];


  if (
    !values.every(Number.isFinite)
  ) {

    throw new Error(
      "Metadata contains invalid geographic bounds."
    );

  }


  return [

    [
      south,
      west
    ],

    [
      north,
      east
    ]

  ];

}


////////////////////////////////////
// READ HEAT OR NDVI RANGE
////////////////////////////////////

function getMetricRange(
  metadata,
  metric
) {

  const metricData =
    metadata?.[metric];


  const minimum =
    Number(metricData?.min);

  const maximum =
    Number(metricData?.max);


  if (
    Number.isFinite(minimum) &&
    Number.isFinite(maximum)
  ) {

    return {
      minimum,
      maximum
    };

  }


  if (metric === "heat") {

    return {
      minimum: 65,
      maximum: 135
    };

  }


  return {
    minimum: -1,
    maximum: 1
  };

}


////////////////////////////////////
// CREATE LEAFLET MAP
////////////////////////////////////

function createLeafletMap(elementId) {

  const map =
    L.map(
      elementId,
      {
        scrollWheelZoom: false,
        zoomControl: true,
        preferCanvas: true,
        attributionControl: true
      }
    );


  L.tileLayer(
    "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    {

      maxZoom: 19,

      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright">' +
        "OpenStreetMap contributors</a>"

    }
  ).addTo(map);


  map.on(
    "mouseout",
    () => {

      map.scrollWheelZoom.disable();

    }
  );


  return map;

}


////////////////////////////////////
// CREATE IMAGE OVERLAY
////////////////////////////////////

function createImageOverlay({
  map,
  imagePath,
  bounds,
  className,
  description,
  zIndex
}) {

  const layer =
    L.imageOverlay(
      imagePath,
      bounds,
      {

        opacity: 0,

        alt: description,

        className,

        interactive: false

      }
    ).addTo(map);


  layer.setZIndex(zIndex);


  return layer;

}


////////////////////////////////////
// ACTIVATE MODE BUTTON
////////////////////////////////////

function activateButton(activeButton) {

  modeButtons.forEach((button) => {

    button.classList.toggle(
      "active",
      button === activeButton
    );

  });

}


////////////////////////////////////
// UPDATE LEGEND
////////////////////////////////////

function setLegend({
  title,
  gradient,
  minimum,
  maximum
}) {

  legendTitle.textContent =
    title;


  legendGradient.style.background =
    gradient;


  legendMin.textContent =
    minimum;


  legendMax.textContent =
    maximum;

}


////////////////////////////////////
// SHOW HEAT
////////////////////////////////////

function showHeat() {

  if (
    !campusHeatLayer ||
    !campusNdviLayer
  ) {

    return;

  }


  currentMode = "heat";


  campusHeatLayer.setOpacity(0.5);

  campusNdviLayer.setOpacity(0);


  activateButton(
    heatButton
  );


  const range =
    getMetricRange(
      campusMetadata,
      "heat"
    );


  setLegend({

    title:
      "Land-surface temperature",

    gradient: `
      linear-gradient(
        to right,
        #000004,
        #320a5e,
        #781c6d,
        #bb3754,
        #ed6925,
        #fbb61a,
        #fcffa4
      )
    `,

    minimum:
      `${Math.round(range.minimum)}°F`,

    maximum:
      `${Math.round(range.maximum)}°F`

  });

}


////////////////////////////////////
// SHOW VEGETATION
////////////////////////////////////

function showNdvi() {

  if (
    !campusHeatLayer ||
    !campusNdviLayer
  ) {

    return;

  }


  currentMode = "ndvi";


  campusHeatLayer.setOpacity(0);

  campusNdviLayer.setOpacity(0.5);


  activateButton(
    ndviButton
  );


  const range =
    getMetricRange(
      campusMetadata,
      "ndvi"
    );


  setLegend({

    title:
      "Vegetation index (NDVI)",

    gradient: `
      linear-gradient(
        to right,
        #f7fcf5,
        #e5f5e0,
        #c7e9c0,
        #a1d99b,
        #74c476,
        #41ab5d,
        #238b45,
        #005a32
      )
    `,

    minimum:
      range.minimum.toFixed(2),

    maximum:
      range.maximum.toFixed(2)

  });

}


////////////////////////////////////
// SHOW BOTH LAYERS
////////////////////////////////////

function showCompare() {

  if (
    !campusHeatLayer ||
    !campusNdviLayer
  ) {

    return;

  }


  currentMode = "compare";


  campusHeatLayer.setOpacity(0.32);

  campusNdviLayer.setOpacity(0.24);


  activateButton(
    compareButton
  );


  setLegend({

    title:
      "Heat with vegetation overlay",

    gradient: `
      linear-gradient(
        to right,
        #313695,
        #74add1,
        #fee090,
        #f46d43,
        #a50026
      )
    `,

    minimum:
      "Cooler",

    maximum:
      "Hotter"

  });

}


////////////////////////////////////
// RESTORE SELECTED MODE
////////////////////////////////////

function refreshCurrentMode() {

  if (currentMode === "ndvi") {

    showNdvi();

    return;

  }


  if (currentMode === "compare") {

    showCompare();

    return;

  }


  showHeat();

}


////////////////////////////////////
// FIND NEAREST RASTER CELL
////////////////////////////////////

function findNearestPixel(
  clickLocation,
  pixelData,
  maximumDistanceMeters = 100
) {

  if (
    !Array.isArray(pixelData) ||
    pixelData.length === 0
  ) {

    return null;

  }


  let nearestPixel = null;

  let nearestDistance = Infinity;


  pixelData.forEach((pixel) => {

    const pixelLocation =
      L.latLng(
        pixel.latitude,
        pixel.longitude
      );


    const distance =
      clickLocation.distanceTo(
        pixelLocation
      );


    if (
      distance <
      nearestDistance
    ) {

      nearestDistance =
        distance;

      nearestPixel =
        pixel;

    }

  });


  if (
    nearestDistance >
    maximumDistanceMeters
  ) {

    return null;

  }


  return {

    ...nearestPixel,

    distanceMeters:
      nearestDistance

  };

}


////////////////////////////////////
// FORMAT POPUP
////////////////////////////////////

function createPixelPopupHtml(
  campusName,
  pixel
) {

  const temperatureText =
    Number.isFinite(pixel.temperature)

      ? `${pixel.temperature.toFixed(1)}°F`

      : "No data";


  const ndviText =
    Number.isFinite(pixel.ndvi)

      ? pixel.ndvi.toFixed(2)

      : "No data";


  const observationText =
    Number.isFinite(
      pixel.validObservations
    )

      ? Math.round(
          pixel.validObservations
        )

      : "Unknown";


  const distanceText =
    Number.isFinite(
      pixel.distanceMeters
    )

      ? `${Math.round(
          pixel.distanceMeters
        )} m`

      : "Unknown";


  return `

    <div class="pixel-popup">

      <span class="pixel-popup-title">
        ${campusName}
      </span>


      <div class="pixel-popup-row">

        <span>
          Surface temperature
        </span>

        <strong>
          ${temperatureText}
        </strong>

      </div>


      <div class="pixel-popup-row">

        <span>
          Vegetation index
        </span>

        <strong>
          ${ndviText}
        </strong>

      </div>


      <p class="pixel-popup-note">

        Approximate value from the nearest
        Landsat raster cell.

        Land-surface temperature is not air temperature.

        <br><br>

        Raster cell distance: ${distanceText}

        <br>

        Valid observations: ${observationText}

      </p>

    </div>

  `;

}


////////////////////////////////////
// SHOW CLICKED PIXEL
////////////////////////////////////

function showPixelInformation({
  map,
  event,
  campusName,
  pixelData
}) {

  const nearestPixel =
    findNearestPixel(
      event.latlng,
      pixelData
    );


  if (!nearestPixel) {

    L.popup({
      maxWidth: 270
    })

      .setLatLng(
        event.latlng
      )

      .setContent(`

        <div class="pixel-popup">

          <span class="pixel-popup-title">
            No nearby raster data
          </span>

          <p class="pixel-popup-note">

            Click inside the colored
            campus raster to view a measurement.

          </p>

        </div>

      `)

      .openOn(map);


    return;

  }


  L.popup({
    maxWidth: 285,
    closeButton: true
  })

    .setLatLng(
      event.latlng
    )

    .setContent(
      createPixelPopupHtml(
        campusName,
        nearestPixel
      )
    )

    .openOn(map);

}


////////////////////////////////////
// REMOVE OLD CAMPUS LAYERS
////////////////////////////////////

function removeCampusLayers() {

  if (
    campusHeatLayer &&
    campusMap.hasLayer(
      campusHeatLayer
    )
  ) {

    campusMap.removeLayer(
      campusHeatLayer
    );

  }


  if (
    campusNdviLayer &&
    campusMap.hasLayer(
      campusNdviLayer
    )
  ) {

    campusMap.removeLayer(
      campusNdviLayer
    );

  }


  campusHeatLayer = null;

  campusNdviLayer = null;

}


////////////////////////////////////
// LOAD SELECTED CAMPUS
////////////////////////////////////

async function updateCampus(
  campusKey
) {

  const campus =
    campusConfig[campusKey];


  if (!campus) {

    throw new Error(
      `Unknown campus: ${campusKey}`
    );

  }


  const requestNumber =
    ++campusChangeRequest;


  campusMapTitle.textContent =
    `Loading ${campus.name}...`;


  mapPanelDescription.textContent =
    "Loading campus";


  campusSelect.disabled =
    true;


  try {

    /*
      Load everything needed for
      the selected campus.
    */

    const results =
      await Promise.all([

        loadJson(
          campus.metadataFile
        ),

        preloadImage(
          campus.heatImage
        ),

        preloadImage(
          campus.ndviImage
        ),

        loadCampusPixelData(
          campus.dataFile
        )

      ]);


    /*
      If the user clicked another campus
      while this one was loading, don't
      overwrite the newer request.
    */

    if (
      requestNumber !==
      campusChangeRequest
    ) {

      return;

    }


    const newMetadata =
      results[0];


    const newPixelData =
      results[3];


    const newBounds =
      getImageBounds(
        newMetadata
      );


    /*
      Remove the old campus.
    */

    removeCampusLayers();


    /*
      Create the new heat layer.
    */

    campusHeatLayer =
      createImageOverlay({

        map: campusMap,

        imagePath:
          campus.heatImage,

        bounds:
          newBounds,

        className:
          "heat-overlay",

        description:
          `${campus.name} land-surface temperature`,

        zIndex: 10

      });


    /*
      Create the new NDVI layer.
    */

    campusNdviLayer =
      createImageOverlay({

        map: campusMap,

        imagePath:
          campus.ndviImage,

        bounds:
          newBounds,

        className:
          "ndvi-overlay",

        description:
          `${campus.name} vegetation index`,

        zIndex: 20

      });


    /*
      Save selected campus data.
    */

    campusMetadata =
      newMetadata;


    campusPixelData =
      newPixelData;


    selectedCampusKey =
      campusKey;


    /*
      Update title.
    */

    campusMapTitle.textContent =
      campus.name;


    mapPanelDescription.textContent =
      "Selected campus";


    /*
      Close any old popup.
    */

    campusMap.closePopup();


    /*
      Zoom the single map to the
      selected campus.
    */

    campusMap.fitBounds(
      newBounds,
      {
        padding: [12, 12],
        animate: false
      }
    );


    /*
      Restore whichever mode the user
      was using.
    */

    refreshCurrentMode();

  }


  finally {

    if (
      requestNumber ===
      campusChangeRequest
    ) {

      campusSelect.disabled =
        false;

    }

  }

}


////////////////////////////////////
// SHOW MAP ERROR
////////////////////////////////////

function showMapError(message) {

  const mapElement =
    document.getElementById(
      "campus-map"
    );


  if (!mapElement) {

    return;

  }


  mapElement.innerHTML = `

    <div class="map-error">

      <div>

        <strong>
          ${message}
        </strong>

        <br><br>

        Check that the campus PNG,
        metadata JSON and temperature
        CSV files exist in web/data.

        <br><br>

        Open index.html using Live Server
        and check the browser console.

      </div>

    </div>

  `;

}


////////////////////////////////////
// START APPLICATION
////////////////////////////////////

async function startMaps() {

  validateHtmlElements();


  /*
    Create ONE map instead of two.
  */

  campusMap =
    createLeafletMap(
      "campus-map"
    );


  /*
    Make sure UCLA is selected initially.
  */

  if (
    campusSelect.value &&
    campusConfig[
      campusSelect.value
    ]
  ) {

    selectedCampusKey =
      campusSelect.value;

  }


  /*
    Load the initial campus.
  */

  await updateCampus(
    selectedCampusKey
  );


  ////////////////////////////////////
  // BUTTON EVENTS
  ////////////////////////////////////

  heatButton.addEventListener(
    "click",
    showHeat
  );


  ndviButton.addEventListener(
    "click",
    showNdvi
  );


  compareButton.addEventListener(
    "click",
    showCompare
  );


  ////////////////////////////////////
  // CAMPUS SELECTION
  ////////////////////////////////////

  campusSelect.addEventListener(
    "change",
    async (event) => {

      const newCampusKey =
        event.target.value;


      try {

        await updateCampus(
          newCampusKey
        );

      }

      catch (error) {

        console.error(error);


        campusMapTitle.textContent =
          campusConfig[
            newCampusKey
          ]?.name ??
          "Selected campus";


        mapPanelDescription.textContent =
          "Could not load campus";


        window.alert(
          "The selected campus could not load. " +
          "Check its PNG, JSON and CSV files."
        );

      }

    }
  );


  ////////////////////////////////////
  // MAP CLICK
  ////////////////////////////////////

  campusMap.on(
    "click",
    (event) => {

      const selectedCampus =
        campusConfig[
          selectedCampusKey
        ];


      if (!selectedCampus) {

        return;

      }


      showPixelInformation({

        map:
          campusMap,

        event,

        campusName:
          selectedCampus.name,

        pixelData:
          campusPixelData

      });

    }
  );


  ////////////////////////////////////
  // RESIZE CORRECTION
  ////////////////////////////////////

  window.addEventListener(
    "resize",
    () => {

      window.requestAnimationFrame(
        () => {

          if (campusMap) {

            campusMap.invalidateSize();

          }

        }
      );

    }
  );


  ////////////////////////////////////
  // INITIAL SIZE CORRECTION
  ////////////////////////////////////

  window.setTimeout(
    () => {

      if (!campusMap) {

        return;

      }


      campusMap.invalidateSize();


      if (campusMetadata) {

        campusMap.fitBounds(
          getImageBounds(
            campusMetadata
          ),
          {
            padding: [12, 12],
            animate: false
          }
        );

      }

    },
    200
  );


  /*
    Start with surface heat.
  */

  showHeat();

}


////////////////////////////////////
// RUN APPLICATION
////////////////////////////////////

startMaps().catch(
  (error) => {

    console.error(error);


    showMapError(
      "The campus map could not load."
    );

  }
);