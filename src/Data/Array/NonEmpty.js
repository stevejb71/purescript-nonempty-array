// module Data.Array.NonEmpty

exports.pop_ = function (l) {
  if (l.length === 0) return l;
  var l1 = l.slice();
  l1.pop();
  return l1;
};
