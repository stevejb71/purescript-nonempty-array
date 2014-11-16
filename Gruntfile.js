module.exports = function(grunt) {
  "use strict";

  grunt.initConfig({ 
    libFiles: [
      "src/**/*.purs",
      "bower_components/*/src/**/*.purs",
    ],
    clean: ["output", "output_tests"],
    pscMake: {
      src: ["<%=libFiles%>"],
      tests: {
        src: ["tests/Main.purs", "<%=libFiles%>"]
      }
    },
    psc: {
      tests: {
        src: ["tests/Main.purs", "<%=libFiles%>"],
        dest: "output_tests/Main.js"
      }
    },
    dotPsci: ["<%=libFiles%>"],
    docgen: {
        readme: {
            src: "src/**/*.purs",
            dest: "README.md"
        }
    },
    execute: {
      tests: {
        src: ["output_tests/Main.js"]
      }
    } 
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  
  grunt.registerTask("make", ["pscMake", "dotPsci", "docgen"]);
  grunt.registerTask("test", ["psc", "execute:tests"]);
  grunt.registerTask("default", ["clean", "make", "test"]);
};