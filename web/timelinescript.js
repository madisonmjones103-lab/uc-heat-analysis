const timelineData = [
  {
    year: "1985",
    image: "timeline/UCLA_1985_MAP.jpg",
    alt: "Historical aerial image of UCLA in 1985"
  },
  {
    year: "1989",
    image: "timeline/UCLA_1989_MAP.jpg",
    alt: "Historical aerial image of UCLA in 1989"
  },
  {
    year: "1994",
    image: "timeline/UCLA_1994_MAP.jpg",
    alt: "Historical aerial image of UCLA in 1994"
  },
  {
    year: "2002",
    image: "timeline/UCLA_2002_MAP.jpg",
    alt: "Historical aerial image of UCLA in 2002"
  },
  {
    year: "2009",
    image: "timeline/UCLA_2009_MAP.jpg",
    alt: "Historical aerial image of UCLA in 2009"
  },
  {
    year: "2015",
    image: "timeline/UCLA_2015_MAP.jpg",
    alt: "Historical aerial image of UCLA in 2015"
  },
  {
    year: "2020",
    image: "timeline/UCLA_2020_MAP.jpg",
    alt: "Historical aerial image of UCLA in 2020"
  },
  {
    year: "2025",
    image: "timeline/UCLA_2025_MAP.jpg",
    alt: "Historical aerial image of UCLA in 2025"
  }
];

const imageElement = document.getElementById("timeline-image");
const imageYear = document.getElementById("image-year");
const yearButtons = document.querySelectorAll(".year-button");

let currentIndex = 0;
let fadeTimeout;

function updateTimeline(index) {
  if (
    index < 0 ||
    index >= timelineData.length ||
    index === currentIndex
  ) {
    return;
  }

  const selectedItem = timelineData[index];

  clearTimeout(fadeTimeout);
  imageElement.classList.add("is-fading");

  fadeTimeout = setTimeout(() => {
    imageElement.src = selectedItem.image;
    imageElement.alt = selectedItem.alt;
    imageYear.textContent = selectedItem.year;

    imageElement.onload = () => {
      imageElement.classList.remove("is-fading");
    };

    if (imageElement.complete) {
      imageElement.classList.remove("is-fading");
    }
  }, 180);

  yearButtons.forEach((button, buttonIndex) => {
    const isActive = buttonIndex === index;

    button.classList.toggle("active", isActive);
    button.setAttribute("aria-pressed", String(isActive));
  });

  currentIndex = index;
}

yearButtons.forEach((button) => {
  button.addEventListener("click", () => {
    const selectedIndex = Number(button.dataset.index);
    updateTimeline(selectedIndex);
  });
});

document.addEventListener("keydown", (event) => {
  if (event.key === "ArrowLeft") {
    const previousIndex = Math.max(currentIndex - 1, 0);

    updateTimeline(previousIndex);
    yearButtons[previousIndex].focus();
  }

  if (event.key === "ArrowRight") {
    const nextIndex = Math.min(
      currentIndex + 1,
      timelineData.length - 1
    );

    updateTimeline(nextIndex);
    yearButtons[nextIndex].focus();
  }
});