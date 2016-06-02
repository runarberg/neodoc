const neodoc = require('../');
const path = require('path');
const $ = require('shelljs');
const expect = require('chai').expect;

const EXAMPLES = path.resolve(__dirname, '..', 'examples');
const GIT_EXAMPLE = path.resolve(EXAMPLES, 'git');

describe('neodoc', () => {
  describe('examples - git', () => {
    const git = (args) => {
      const p = $.exec(`node "${GIT_EXAMPLE}" ${args}`, { silent: true });
      if (p.code === 0) {
        return JSON.parse(p.stdout);
      } else {
        throw new Error(p.stdout);
      }
    }

    it('git branch -u origin master', () => {
      expect(git('branch -u origin master'))
        .to.deep.equal({
          branch: true
        , '<branchname>': [ 'master' ]
        , '-u': 'origin'
        , '--set-upstream-to': 'origin'
        });
    });

    it('git clone --separate-git-dir=~/foo', () => {
      expect(git('clone --separate-git-dir=~/foo'))
        .to.deep.equal({
          '--': []
        , '--separate-git-dir': '~/foo'
        , 'clone': true
        });
    });

    it('git commit --gpg-sign=my-key', () => {
      expect(git('commit --gpg-sign=my-key'))
        .to.deep.equal({
          '--': []
        , 'commit': true
        , '--gpg-sign': 'my-key'
        , '-S': 'my-key'
        });
    });
  });
});
