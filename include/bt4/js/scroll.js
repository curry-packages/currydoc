// Handles scrolling and active link highlighting for section navigation.
//
// TODO: Because of the nested nature of sections, we can't properly
//       calculate bounding boxes for each section seperately, and 
//       therefore scrolling up will not highlight the correct link 
//       until the section title is visible.
document.addEventListener('DOMContentLoaded', () => {
  const navLinks = document.querySelectorAll('.nav-link');
  const sections = document.querySelectorAll('section[id]');

  const activateLink = (id) => {
    navLinks.forEach(link => {
      link.classList.toggle('active', link.getAttribute('href') === `#${id}`);
    });
  };

  const onScroll = () => {
    let currentSectionId = null;
    let minOffset = window.innerHeight;

    sections.forEach(section => {
      const rect = section.getBoundingClientRect();
      if (rect.top >= 0 && rect.top < minOffset) {
        minOffset = rect.top;
        currentSectionId = section.id;
      }
    });

    if (currentSectionId) {
      activateLink(currentSectionId);
    }
  };

  onScroll();

  window.addEventListener('scroll', () => {
    window.requestAnimationFrame(onScroll);
  });
});
