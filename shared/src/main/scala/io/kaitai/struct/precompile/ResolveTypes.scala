package io.kaitai.struct.precompile

import io.kaitai.struct.Log
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{EnumType, SwitchType, UserType}
import io.kaitai.struct.format._

/**
  * A collection of methods that resolves user types and enum types, i.e.
  * converts names into ClassSpec / EnumSpec references.
  */
class ResolveTypes(specs: ProtocolSpecs, opaqueTypes: Boolean) {
  def run(): Unit = specs.forEachRec(resolveUserTypes)

  /**
    * Resolves user types and enum types recursively starting from a certain
    * ClassSpec.
    * @param curClass class to start from, might be top-level class
    */
  def resolveUserTypes(curClass: ClassSpec): Unit = {
    curClass match {
      case curClass: ClassWithSeqSpec =>
        curClass.seq.foreach((attr) => resolveUserTypeForMember(curClass, attr))
      case _ =>
    }
    curClass match {
      case curClass: StructSpec =>
        curClass.instances.foreach { case (_, instSpec) =>
          instSpec match {
            case pis: ParseInstanceSpec =>
              resolveUserTypeForMember(curClass, pis)
            case _: ValueInstanceSpec =>
            // ignore all other types of instances
          }
        }
        curClass.params.foreach((paramDef) => resolveUserTypeForMember(curClass, paramDef))
      case curClass: ProtocolSpec =>
        curClass.stateMachine.states.foreach { case (src, dst) =>
          dst.foreach { case (dst, ut) =>
            resolveUserType(curClass, ut, curClass.path ++ List(src, dst) ++ ut.name)
          }
        }
      case _ => // Nothing else, for now
    }
  }

  def resolveUserTypeForMember(curClass: ClassSpec, attr: MemberSpec): Unit =
    resolveUserType(curClass, attr.dataType, attr.path)

  def resolveUserType(curClass: ClassSpec, dataType: DataType, path: List[String]): Unit = {
    dataType match {
      case ut: UserType =>
        ut.classSpec = resolveUserType(curClass, ut.name, path)
      case et: EnumType =>
        et.enumSpec = resolveEnumSpec(curClass, et.name)
        if (et.enumSpec.isEmpty) {
          val err = new EnumNotFoundError(et.name.mkString("::"), curClass)
          throw new YAMLParseException(err.getMessage, path)
        }
      case st: SwitchType =>
        st.cases.foreach { case (caseName, ut) =>
          resolveUserType(curClass, ut, path ++ List("type", "cases", caseName.toString))
        }
      case _ =>
        // not a user type, nothing to resolve
    }
  }

  def resolveUserType(curClass: ClassSpec, typeName: List[String], path: List[String]): Option[ClassSpec] = {
    Log.typeResolve.info(() => s"resolveUserType: at ${curClass.name} doing ${typeName.mkString("|")}")
    val res = realResolveUserType(curClass, typeName, path)

    res match {
      case None =>
        // Type definition not found
        if (opaqueTypes) {
          // Generate special "opaque placeholder" ClassSpec
          Log.typeResolve.info(() => "    => ??? (generating opaque type)")
          Some(StructSpec.opaquePlaceholder(typeName))
        } else {
          // Opaque types are disabled => that is an error
          val err = new TypeNotFoundError(typeName.mkString("::"), curClass)
          throw new YAMLParseException(err.getMessage, path)
        }
      case Some(x) =>
        Log.typeResolve.info(() => s"    => ${x.nameAsStr}")
        res
    }
  }

  private def realResolveUserType(curClass: ClassSpec, typeName: List[String], path: List[String]): Option[ClassSpec] = {
    // First, try to do it in current class

    // If we're seeking composite name, we only have to resolve the very first
    // part of it at this stage
    val firstName :: restNames = typeName


    // TODO FIXME might need to add support for InputSpecs as well
    val resolvedHere = curClass.types.get(firstName).flatMap((nestedClass) =>
      if (restNames.isEmpty) {
        // No further names to resolve, here's our answer
        Some(nestedClass)
      } else {
        // Try to resolve recursively
        resolveUserType(nestedClass, restNames, path)
      }
    )

    resolvedHere match {
      case Some(_) => resolvedHere
      case None =>
        val resolvedHere2 = curClass match {
          case curClass: ProtocolSpec =>
            curClass.inputs.get(firstName).flatMap((nestedClass) =>
              if (restNames.isEmpty) {
                // No further names to resolve, here's our answer
                Some(nestedClass)
              } else {
                None
              }
            )
          case _ => None
        }
        resolvedHere2 match {
          case Some(_) => resolvedHere2
          case _ =>
            // No luck resolving here, let's try upper levels, if they exist
            curClass.upClass match {
              case Some(upClass) =>
                resolveUserType(upClass, typeName, path)
              case None =>
                // Check this class if it's top-level class
                if (curClass.name.head == firstName) {
                  curClass match {
                    case curClass: StructSpec => Some(curClass)
                    case _ => None
                  }
                } else {
                  // Check if top-level specs has this name
                  // If there's None => no luck at all
                  val topLevelCandidates = specs.values.filter(ps => ps.types.contains(firstName)) // || ps.inputs.contains(firstName)
                  val resolvedTop = topLevelCandidates.head.types.get(firstName)
                  resolvedTop match {
                    case Some(classSpec: StructSpec) => if (restNames.isEmpty) {
                      Some(classSpec)
                    } else {
                      resolveUserType(classSpec, restNames, path)
                    }
                    case _ => None
                  }
                }
            }
        }
    }
  }

  def resolveEnumSpec(curClass: ClassSpec, typeName: List[String]): Option[EnumSpec] = {
    //    Console.println(s"resolveEnumSpec: at ${curClass.name} doing ${typeName.mkString("|")}")
    val res = realResolveEnumSpec(curClass, typeName)
    //    Console.println("   => " + (res match {
    //      case None => "???"
    //      case Some(x) => x.name.mkString("|")
    //    }))

    res
  }

  private def realResolveEnumSpec(curClass: ClassSpec, typeName: List[String]): Option[EnumSpec] = {
    curClass match {
      case curClass: StructSpec =>
        // First, try to do it in current class

        // If we're seeking composite name, we only have to resolve the very first
        // part of it at this stage
        val firstName :: restNames = typeName

        val resolvedHere = if (restNames.isEmpty) {
          curClass.enums.get(firstName)
        } else {
          curClass.types.get(firstName).flatMap((nestedClass) =>
            resolveEnumSpec(nestedClass, restNames)
          )
        }

        resolvedHere match {
          case Some(_) => resolvedHere
          case None =>
            // No luck resolving here, let's try upper levels, if they exist
            curClass.upClass match {
              case Some(upClass: StructSpec) =>
                resolveEnumSpec(upClass, typeName)
              case _ =>
                // No luck at all
                None
            }
        }
      case _ => None
    }
  }
}
