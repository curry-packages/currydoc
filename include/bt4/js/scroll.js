// Handles scrolling and active link highlighting for section navigation.
document.addEventListener('DOMContentLoaded', () => {
  const navLinks = document.querySelectorAll('.nav-link');
  const sections = document.querySelectorAll('section[id]');
  
  let recentlyClickedSection = null;
  let clickTimeout = null;

  const activateLink = (id) => {
    navLinks.forEach(link => {
      link.classList.toggle('active', link.getAttribute('href') === `#${id}`);
    });
  };

  // Pixels from top of viewport before section becomes active
  // TODO: It might be a good idea to either make this configurable
  //       or use a more dynamic approach (e.g., based on ratios) 
  //       to determine when a section should be considered active.
  const ACTIVATION_OFFSET = 500; 

  const onScroll = () => {
    // Check if recently clicked section is still active
    if (recentlyClickedSection) {
      const clickedSection = document.getElementById(recentlyClickedSection);
      if (clickedSection) {
        const rect = clickedSection.getBoundingClientRect();
        // Keep the clicked section active if it's still visible
        if (rect.bottom > 0 && rect.top < window.innerHeight) {
          return;
        }
      }

      recentlyClickedSection = null;
      if (clickTimeout) {
        clearTimeout(clickTimeout);
        clickTimeout = null;
      }
    }

    let currentSectionId = null;
    
    // Find the section that should be active based on scroll position
    // A section becomes active when it crosses the activation threshold
    sections.forEach(section => {
      const rect = section.getBoundingClientRect();
      
      if (rect.top <= ACTIVATION_OFFSET && rect.bottom > ACTIVATION_OFFSET) {
        currentSectionId = section.id;
      }
    });

    if (currentSectionId) {
      activateLink(currentSectionId);
    }
  };

  // Handle nav link clicks
  navLinks.forEach(link => {
    link.addEventListener('click', (e) => {
      const href = link.getAttribute('href');
      if (href && href.startsWith('#')) {
        const sectionId = href.substring(1);
        recentlyClickedSection = sectionId;
        activateLink(sectionId);
        
        if (clickTimeout) {
          clearTimeout(clickTimeout);
        }
        
        clickTimeout = setTimeout(() => {
          recentlyClickedSection = null;
        }, 2000);
      }
    });
  });

  onScroll();

  window.addEventListener('scroll', () => {
    window.requestAnimationFrame(onScroll);
  });
});