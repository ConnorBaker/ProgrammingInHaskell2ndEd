module.exports = {
  content: ["./docs/**/*.{html,js}"],
  darkMode: "media",
  theme: {
    extend: {
      backgroundImage: {
        bumblebee: "url('../images/bumblebee.jpeg')",
      },
    },
    fontFamily: {
      display: ['"Playfair Display"', "serif"],
      serif: ['"Source Serif Pro"', "serif"],
      sans: ['"Source Sans Pro"', "sans-serif"],
      mono: ['"Fira Code"', "monospace"],
    },
  },
};
