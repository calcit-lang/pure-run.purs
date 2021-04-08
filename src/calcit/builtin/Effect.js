// dirty thing
exports.requireEvalImpl = (file) => (
  name
) => (/* extra level for Effect */) => {
  let m = require(file);
  console.log("module", m);
  m[name]();
};
