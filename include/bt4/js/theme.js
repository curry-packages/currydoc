document.addEventListener('DOMContentLoaded', function() {
  // Check for saved theme preference or use prefers-color-scheme
  const savedTheme = localStorage.getItem('theme') || 
                    (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
  
  // Apply the saved theme
  setTheme(savedTheme);
  
  // Theme toggle button functionality
  const themeToggle = document.getElementById('theme-toggle');
  if (themeToggle) {
    themeToggle.addEventListener('click', function() {
      const currentTheme = document.documentElement.getAttribute('data-theme') || 'light';
      const newTheme = currentTheme === 'light' ? 'dark' : 'light';
      setTheme(newTheme);
      localStorage.setItem('theme', newTheme);
    });
  }
});

function setTheme(theme) {
  document.documentElement.setAttribute('data-theme', theme);
}