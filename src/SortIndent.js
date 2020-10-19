const { chunkType } = require('./CommentsAndStrings')



// TODO replace this function whereever it is used with something less dumb
function chunkToString(c) {
 return inp.slice(c.start, c.end);
}



function searchTowardsRoot(leafToAdd, targetLeaf) {

    if (leafToAdd.indent < targetLeaf.indent) {
      if (leafToAdd.indent > targetLeaf.parent.indent) {
        throw new Error('bad indent!');
      }

      searchTowardsRoot(leafToAdd, targetLeaf.parent);

    } else if (leafToAdd.indent === targetLeaf.indent) {
      append(leafToAdd, targetLeaf.parent);
    } else {
      append(leafToAdd, targetLeaf);
    }
  }

function append(leafToAdd, parent) {
    leafToAdd.parent = parent;
    parent.children.push(leafToAdd);
  }



function getLineIndent(chunks) {
    // TODO find a less dumb algorithm
    let l = chunks.map(chunkToString).join('');
    for (var i = 0; l[i] === ' '; i++);
    return i;
  }



function chunkIsEmptyOrComment(chunk) {
  return (chunk.type !== chunkType.ContentLine) || /^[ ]*\n?$/.test(chunkToString(chunk));
}


function sortIndent(lines) {

  let root = {
    chunks: [],
    parent: null,
    indent: -1,
    // TODO make clear that these lines *precede* the leaf?
    commentLines: [],
    children: [],
  };

  let lastAdded = root;
  let commentLinesAccumulator = [];

  lines.forEach(chunks => {
    if (chunks.every(chunkIsEmptyOrComment)) {
      commentLinesAccumulator.push(chunks);
    } else {

      let leaf = {
        chunks: chunks,
        indent: getLineIndent(chunks),
        parent: null,
        commentLines: [],
        children: [],
      };

      searchTowardsRoot(leaf, lastAdded);
      leaf.commentLines = commentLinesAccumulator;
      commentLinesAccumulator = [];
      lastAdded = leaf;
    }
  });

  return root;
}


//
// Exports
//
module.exports = { sortIndent }
