module.exports = function(grunt) {
  "use strict";

  grunt.initConfig({ 
    libFiles: [
      "src/**/*.purs",
      "bower_components/*/src/**/*.purs",
    ],
    testFiles: [
      "tests/*.purs"
    ],
    clean: ["output", "output_tests"],
    pscMake: {
      src: ["<%=libFiles%>"],
      tests: {
        src: ["<%=testFiles%>", "<%=libFiles%>"]
      }
    },
    psc: {
      tests: {
        options: {
          module: ["Main"],
          main: true
        },        
        src: ["<%=testFiles%>", "<%=libFiles%>"],
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