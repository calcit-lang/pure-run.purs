// dirty thing
exports.requireEvalImpl = (file) => (name) => () => {
  /* extra fn wrapper since Effect */
  let m = require(file);
  console.log("module", m);
  m[name]();
};
