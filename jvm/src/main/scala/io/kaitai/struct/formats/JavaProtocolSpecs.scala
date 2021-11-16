package io.kaitai.struct.formats

import java.io.{File, FileNotFoundException, IOError}
import io.kaitai.struct.Log
import io.kaitai.struct.format.{StructSpec, ClassSpec, ProtocolSpecs, ProtocolSpec}
import io.kaitai.struct.precompile.ErrorInInput

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Java implementation of ClassSpec container, doing imports from local files.
  */
class JavaProtocolSpecs(relPath: String, absPaths: Seq[String], firstSpec: ProtocolSpec)
  extends ProtocolSpecs(firstSpec) {

  private val relFiles = mutable.Map[String, ProtocolSpec]()
  private val absFiles = mutable.Map[String, ProtocolSpec]()

  override def importRelative(name: String, path: List[String], inFile: Option[String]): Future[Option[ProtocolSpec]] = Future {
    Log.importOps.info(() => s".. importing relative $name")
    JavaProtocolSpecs.cached(path, inFile, relFiles, name, (_) =>
      JavaKSYParser.fileNameToSpec(s"$relPath/$name.ksy")
    )
  }

  override def importAbsolute(name: String, path: List[String], inFile: Option[String]): Future[Option[ProtocolSpec]] = Future {
    Log.importOps.info(() => s".. importing absolute $name")
    JavaProtocolSpecs.cached(path, inFile, absFiles, name, tryAbsolutePaths)
  }

  def tryAbsolutePaths(name: String): ProtocolSpec = {
    absPaths.foreach { (path) =>
      val fn = s"$path/$name.ksy"
      val f = new File(fn)
      if (f.exists) {
        if (f.canRead) {
          if (f.isFile) {
            return JavaKSYParser.fileNameToSpec(fn)
          } else {
            Log.importOps.warn(() => s".... $fn exists, but is not a regular file, skipping")
          }
        } else {
          Log.importOps.warn(() => s".... $fn exists, but not readable, skipping")
        }
      }
    }
    throw new FileNotFoundException(s"Unable to find '$name' in import search paths, using: $absPaths")
  }
}

object JavaProtocolSpecs {
  def cached(
    path: List[String],
    inFile: Option[String],
    cacheMap: mutable.Map[String, ProtocolSpec],
    name: String,
    importOp: (String) => ProtocolSpec
  ): Option[ProtocolSpec] = {
    // Have we loaded it previously?
    cacheMap.get(name) match {
      case Some(_) =>
        // Yes, it's already loaded and processed, nothing new here
        Log.importOps.info(() => s".... cached")
        None
      case None =>
        // Nope, let's import it
        try {
          val spec = importOp(name)
          cacheMap(name) = spec
          Some(spec)
        } catch {
          case err: Throwable => throw new ErrorInInput(err, path, inFile)
        }
    }
  }
}
