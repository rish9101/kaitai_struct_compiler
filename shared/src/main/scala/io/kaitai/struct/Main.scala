package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, GenericSpecStructClass$, ProtocolSpec, ProtocolSpecs, StructSpec}
import io.kaitai.struct.languages.{GoCompiler, RustCompiler}
import io.kaitai.struct.languages.components.LanguageCompilerStatic
import io.kaitai.struct.precompile._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  /**
    * Takes a freshly made [[ProtocolSpecs]] container with a single .ksy loaded
    * into it, launches recursive loading of imports into this container,
    * and then runs precompilation on every class that happens to be there
    * after imports.
    *
    * @param specs ClassSpecs container with first class loaded into it
    * @param config runtime configuration to be passed to precompile step
    * @return a future that will resolve when both imports and precompilations
    *         are complete; modifies given container by loading extra classes
    *         into it and modifying classes itself by precompilation step
    */
  def importAndPrecompile(specs: ProtocolSpecs, config: RuntimeConfig): Future[Unit] = {
    new LoadImports(specs).processClass(specs.firstSpec, LoadImports.BasePath).map { (allSpecs) =>
      Log.importOps.info(() => s"imports done, got: ${specs.keys} (async=$allSpecs)")

      specs.foreach { case (_, classSpec) =>
        precompile(specs, classSpec, config)
      }
    }
  }

  def precompile(classSpecs: ProtocolSpecs, topClass: ClassSpec, config: RuntimeConfig): Unit = {
    classSpecs.foreach { case (_, curClass) => curClass.markupClassNames }
    val opaqueTypes = topClass.meta.opaqueTypes.getOrElse(config.opaqueTypes)
    new ResolveTypes(classSpecs, opaqueTypes).run()
    new ParentTypes(classSpecs).run()
    new SpecsValueTypeDerive(classSpecs).run()
    new CalculateSeqSizes(classSpecs).run()
    new TypeValidator(classSpecs, topClass).run()

    topClass.types.foreach { case (_, topClass) => topClass.parentClass = GenericSpecStructClass$ }
  }

  /**
    * Compiles a single [[ProtocolSpec]] into a single target language using
    * provided configuration.
    *
    * @param specs bundle of class specifications (used to search to references there)
    * @param spec class specification to compile
    * @param lang specifies which language compiler will be used
    * @param conf runtime compiler configuration
    * @return a container that contains all compiled files and results
    */
  def compile(specs: ProtocolSpecs, spec: ProtocolSpec, lang: LanguageCompilerStatic, conf: RuntimeConfig): CompileLog.SpecSuccess = {
    val config = updateConfig(conf, spec)

    val cc = lang match {
      case GraphvizClassCompiler =>
        new GraphvizClassCompiler(specs, spec)
      case GoCompiler =>
        new GoClassCompiler(specs, spec, config)
      case RustCompiler =>
        new RustClassCompiler(specs, spec, config)
      case ConstructClassCompiler =>
        new ConstructClassCompiler(specs, spec)
      case HtmlClassCompiler =>
        new HtmlClassCompiler(specs, spec)
      case NimClassCompiler =>
        new NimClassCompiler(specs, spec, config)
      case _ =>
        new ClassCompiler(specs, spec, config, lang)
    }
    cc.compile
  }

  /**
    * Updates runtime configuration with "enforcement" options that came from a source file itself.
    * Currently only used to enforce debug when "ks-debug: true" is specified in top-level "meta" key.
    * @param config original runtime configuration
    * @param topClass top-level class spec
    * @return updated runtime configuration with applied enforcements
    */
  private def updateConfig(config: RuntimeConfig, topClass: ProtocolSpec): RuntimeConfig = {
    if (topClass.meta.forceDebug) {
      config.copy(autoRead = false, readStoresPos = true)
    } else {
      config
    }
  }
}
