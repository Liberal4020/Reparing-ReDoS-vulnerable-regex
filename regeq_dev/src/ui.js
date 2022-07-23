var to_NFA = require('./regex');

// function $(s){
//   return document.querySelector(s);
// }

// function hideAll() {
//   $('#equiv-div').style.display = 'none';
//   $('#error-div').style.display = 'none';
//   $('#unequiv-div').style.display = 'none';
//   $('#both-error').style.display = 'none';
//   $('#first-error').style.display = 'none';
//   $('#second-error').style.display = 'none';
//   $('#first-accept-div').style.display = 'none';
//   $('#second-accept-div').style.display = 'none';
// }

// function show(ele) {
//   ele.style.display = 'inline';
// }

function parse(s1) {
  try {
    return to_NFA(s1);
  } catch(e) {
    return null;
  }
}

function union(l1, l2) {
  return l1.concat(l2).filter(function(v, i, a){return a.indexOf(v) === i}).sort();
}

function update() {
  // hideAll();

  var m1 = parse(lines[0].replace(/\s/g, ''));
  var m2 = parse(lines[1].replace(/\s/g, ''));

  if (!m1 || !m2) {
    if (!m1 && !m2) {
      console.log('Couldn\'t parse either regex');
    } else if (!m1) {
      console.log('Couldn\'t parse first regex');
    } else {
      console.log('Couldn\'t parse second regex');
    }
    return;
  }

  m1.alphabet = m2.alphabet = union(m1.alphabet, m2.alphabet);
  m1 = m1.minimized();
  m2 = m2.minimized();

  var strings = m1.find_equivalence_counterexamples(m2);

  if (strings[0] === null && strings[1] === null) {
    console.log('Equivalent');
  } else {
    strings = strings.map(function(s){return s === '' ? '[empty string]' : s});

    if (strings[0] !== null) {
      console.log('Accepted by first but not second: ' + strings[0]);
    }

    if (strings[1] !== null) {
      console.log('Accepted by second but not first: ' + strings[1]);
    }
  }
}

var lines = [];
var reader = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
});
reader.on('line', function(line) {
  lines.push(line);
});
reader.on('close', function() {
  update();
});
