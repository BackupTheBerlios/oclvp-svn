<?xml version="1.0"?>
<!--

xmi2suml.xsl - convert UML models in XMI 1.0 to SUML 0.2

This file is part of oclvp.

Copyright (c) 2005 by Marcel Kyas

Original version: Copyright (c) 2003 Joost Jacob <Joost.Jacob@cwi.nl>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml"
              doctype-public="-//Marcel Kyas//DTD suml XML 0.2//EN"
              doctype-system="http://oclvp.berlios.de/suml02.dtd"
              indent="yes" />
  <xsl:variable name="modelname">
    <xsl:apply-templates select="/XMI/XMI.content/Model_Management.Model"/>
  </xsl:variable>
  <xsl:template match="Model_Management.Model">
    <xsl:value-of select="Foundation.Core.ModelElement.name"/>
  </xsl:template>
  <xsl:variable name="exportername">
    <xsl:value-of select="/XMI/XMI.header/XMI.documentation/XMI.exporter"/>
  </xsl:variable>
  <xsl:variable name="exporterversion">
    <xsl:value-of select="/XMI/XMI.header/XMI.documentation/XMI.exporterVersion"/>
  </xsl:variable>
  <xsl:template match="Foundation.Core.Class|Foundation.Core.DataType|Foundation.Core.Interface">
    <class>
      <xsl:attribute name="name">
        <xsl:value-of select="Foundation.Core.ModelElement.name"/>
      </xsl:attribute>
      <xsl:attribute name="kind">
        <xsl:choose>
          <xsl:when test="Foundation.Core.Class.isActive/@xmi.value='true'">active</xsl:when>
          <xsl:otherwise>passive</xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:call-template name="supertypes"/>
      <xsl:call-template name="attributes"/>
      <xsl:call-template name="operations"/>
      <xsl:call-template name="signals"/>
      <xsl:apply-templates select="Foundation.Core.Namespace.ownedElement/Behavioral_Elements.State_Machines.StateMachine"/>
      <xsl:call-template name="constraints"/>
    </class>
  </xsl:template>
  <xsl:key name="constraint" match="//Foundation.Core.Constraint" use="@xmi.id"/>
  <xsl:template name="constraints">
    <xsl:variable name="constraintNodes" select="Foundation.Core.ModelElement.constraint"/>
    <xsl:if test="count($constraintNodes) &gt; 0">
      <xsl:for-each select="$constraintNodes">
        <xsl:variable name="thisConstraint" select="key('constraint', Foundation.Core.Constraint/@xmi.idref)"/>
        <xsl:apply-templates select="$thisConstraint"/>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:template match="Foundation.Core.Constraint">
    <constraint>
      <xsl:attribute name="lang">
        <xsl:value-of select="Foundation.Core.Constraint.body/Foundation.Data_Types.BooleanExpression/Foundation.Data_Types.Expression.language"/>
      </xsl:attribute>
      <xsl:variable name="name" select="Foundation.Core.ModelElement.name"/>
      <xsl:if test="$name!=''">
        <xsl:attribute name="name">
          <xsl:value-of select="$name"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:value-of select="Foundation.Core.Constraint.body/Foundation.Data_Types.BooleanExpression/Foundation.Data_Types.Expression.body"/>
    </constraint>
  </xsl:template>
  <xsl:template match="Foundation.Core.Namespace.ownedElement/Behavioral_Elements.State_Machines.StateMachine">
    <statemachine>
      <region>
        <xsl:variable name="states" select="Behavioral_Elements.State_Machines.StateMachine.top/Behavioral_Elements.State_Machines.CompositeState/Behavioral_Elements.State_Machines.CompositeState.subvertex"/>
        <xsl:apply-templates select="$states/Behavioral_Elements.State_Machines.Pseudostate|$states/Behavioral_Elements.State_Machines.State"/>
        <xsl:apply-templates select="Behavioral_Elements.State_Machines.StateMachine.transitions/Behavioral_Elements.State_Machines.Transition"/>
      </region>
    </statemachine>
  </xsl:template>
  <xsl:template match="Behavioral_Elements.State_Machines.Transition">
    <transition>
      <xsl:attribute name="source">
        <xsl:value-of select="Behavioral_Elements.State_Machines.Transition.source/Behavioral_Elements.State_Machines.StateVertex/@xmi.idref|Behavioral_Elements.State_Machines.Transition.source/Behavioral_Elements.State_Machines.Pseudostate/@xmi.idref|Behavioral_Elements.State_Machines.Transition.source/Behavioral_Elements.State_Machines.State/@xmi.idref"/>
      </xsl:attribute>
      <xsl:attribute name="target">
        <xsl:value-of select="Behavioral_Elements.State_Machines.Transition.target/Behavioral_Elements.State_Machines.StateVertex/@xmi.idref|Behavioral_Elements.State_Machines.Transition.target/Behavioral_Elements.State_Machines.Pseudostate/@xmi.idref|Behavioral_Elements.State_Machines.Transition.target/Behavioral_Elements.State_Machines.State/@xmi.idref"/>
      </xsl:attribute>
<!-- <xsl:variable name="async" select="Behavioral_Elements.State_Machines.Transition.effect/Behavioral_Elements.Common_Behavior.CallAction/Behavioral_Elements.Common_Behavior.Action.isAsynchronous/@xmi.value" />
    <xsl:if test="$async!=''">
        <xsl:attribute name="isAsynchronous">
            <xsl:value-of select="$async"/>
        </xsl:attribute>
    </xsl:if> -->
      <xsl:apply-templates select="Behavioral_Elements.State_Machines.Transition.trigger"/>
      <xsl:apply-templates select="Behavioral_Elements.State_Machines.Transition.guard"/>
      <xsl:apply-templates select="Behavioral_Elements.State_Machines.Transition.effect"/>
    </transition>
  </xsl:template>
  <xsl:key name="triggerkey" match="//Behavioral_Elements.State_Machines.CallEvent|//Behavioral_Elements.State_Machines.SignalEvent" use="@xmi.id"/>
  <xsl:template name="triggerName">
    <xsl:param name="target"/>
    <xsl:value-of select="key('triggerkey', $target)/Behavioral_Elements.State_Machines.CallEvent.operation/Foundation.Core.Operation/@xmi.idref|key('triggerkey', $target)/Behavioral_Elements.State_Machines.SignalEvent.signal/Behavioral_Elements.Common_Behavior.Signal/@xmi.idref"/>
  </xsl:template>
  <xsl:template match="Behavioral_Elements.State_Machines.Transition.trigger">
    <xsl:variable name="target" select="Behavioral_Elements.State_Machines.Event/@xmi.idref|Behavioral_Elements.State_Machines.CallEvent/@xmi.idref|Behavioral_Elements.State_Machines.SignalEvent/@xmi.idref"/>
    <xsl:variable name="foo">
      <xsl:call-template name="triggerName">
        <xsl:with-param name="target" select="$target"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:if test="$foo!=''">
      <trigger>
        <xsl:attribute name="idref">
          <xsl:value-of select="$foo"/>
        </xsl:attribute>
      </trigger>
    </xsl:if>
  </xsl:template>
  <xsl:template match="Behavioral_Elements.State_Machines.Transition.guard">
    <guard>
      <xsl:attribute name="lang">
        <xsl:value-of select="Behavioral_Elements.State_Machines.Guard/Behavioral_Elements.State_Machines.Guard.expression/Foundation.Data_Types.BooleanExpression/Foundation.Data_Types.Expression.language"/>
      </xsl:attribute>
      <xsl:value-of select="Behavioral_Elements.State_Machines.Guard/Behavioral_Elements.State_Machines.Guard.expression/Foundation.Data_Types.BooleanExpression/Foundation.Data_Types.Expression.body"/>
    </guard>
  </xsl:template>
  <xsl:template match="Behavioral_Elements.State_Machines.Transition.effect">
    <action>
      <xsl:attribute name="lang">
        <xsl:value-of select="Behavioral_Elements.Common_Behavior.CallAction/Behavioral_Elements.Common_Behavior.Action.script/Foundation.Data_Types.ActionExpression/Foundation.Data_Types.Expression.language"/>
      </xsl:attribute>
      <xsl:value-of select="Behavioral_Elements.Common_Behavior.CallAction/Behavioral_Elements.Common_Behavior.Action.script/Foundation.Data_Types.ActionExpression/Foundation.Data_Types.Expression.body|Behavioral_Elements.Common_Behavior.UninterpretedAction/Behavioral_Elements.Common_Behavior.Action.script/Foundation.Data_Types.ActionExpression/Foundation.Data_Types.Expression.body"/>
    </action>
  </xsl:template>
  <xsl:template match="Behavioral_Elements.State_Machines.Pseudostate|Behavioral_Elements.State_Machines.State">
    <state>
      <xsl:variable name="name" select="Foundation.Core.ModelElement.name"/>
      <xsl:if test="$name!=''">
        <xsl:attribute name="name">
          <xsl:value-of select="$name"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:attribute name="id">
        <xsl:value-of select="./@xmi.id"/>
      </xsl:attribute>
      <xsl:variable name="kind" select="Behavioral_Elements.State_Machines.Pseudostate.kind/@xmi.value"/>
      <xsl:if test="$kind='initial'">
        <xsl:attribute name="kind">
          <xsl:value-of select="$kind"/>
        </xsl:attribute>
      </xsl:if>
    </state>
  </xsl:template>
  <xsl:template match="Foundation.Core.AssociationEnd">
    <associationend>
      <xsl:variable name="name" select="Foundation.Core.ModelElement.name"/>
      <xsl:if test="$name!=''">
        <xsl:attribute name="name">
          <xsl:value-of select="$name"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:attribute name="aggregation">
        <xsl:choose>
          <xsl:when test="Foundation.Core.AssociationEnd.aggregation/@xmi.value='shared'">aggregate</xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="Foundation.Core.AssociationEnd.aggregation/@xmi.value"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:variable name="navigable" select="Foundation.Core.AssociationEnd.isNavigable/@xmi.value"/>
      <xsl:if test="$navigable!=''">
        <xsl:attribute name="isnavigable">
          <xsl:value-of select="$navigable"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:attribute name="class">
        <xsl:variable name="target" select="Foundation.Core.AssociationEnd.type/*/@xmi.idref"/>
        <xsl:call-template name="classifierName">
          <xsl:with-param name="target" select="$target"/>
        </xsl:call-template>
      </xsl:attribute>
    </associationend>
  </xsl:template>
  <xsl:template match="Foundation.Core.Association">
    <association>
      <xsl:variable name="name" select="Foundation.Core.ModelElement.name"/>
      <xsl:if test="$name!=''">
        <xsl:attribute name="name">
          <xsl:value-of select="$name"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="Foundation.Core.Association.connection/Foundation.Core.AssociationEnd"/>
    </association>
  </xsl:template>
  <xsl:key name="classifier" match="//Foundation.Core.Class|//Foundation.Core.Interface|//Foundation.Core.DataType" use="@xmi.id"/>
  <xsl:template name="classifierName">
    <xsl:param name="target"/>
    <xsl:value-of select="key('classifier', $target)/Foundation.Core.ModelElement.name"/>
  </xsl:template>
  <xsl:key name="generalization" match="//Foundation.Core.Generalization" use="@xmi.id"/>
  <xsl:template name="supertypes">
    <xsl:variable name="generalizations" select="Foundation.Core.GeneralizableElement.generalization/Foundation.Core.Generalization"/>
    <xsl:if test="count($generalizations) &gt; 0">
      <xsl:for-each select="$generalizations">
        <extends>
          <xsl:attribute name="name">
            <xsl:variable name="generalization" select="key('generalization', ./@xmi.idref)"/>
            <xsl:variable name="target" select="$generalization/Foundation.Core.Generalization.parent/*/@xmi.idref"/>
            <xsl:call-template name="classifierName">
              <xsl:with-param name="target" select="$target"/>
            </xsl:call-template>
          </xsl:attribute>
        </extends>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:template name="attributes">
    <xsl:variable name="attributes" select="Foundation.Core.Classifier.feature/Foundation.Core.Attribute"/>
    <xsl:if test="count($attributes) &gt; 0">
      <xsl:for-each select="$attributes">
        <attribute>
          <xsl:attribute name="name">
            <xsl:value-of select="Foundation.Core.ModelElement.name"/>
          </xsl:attribute>
          <xsl:attribute name="type">
            <xsl:variable name="target" select="Foundation.Core.StructuralFeature.type/*/@xmi.idref"/>
            <xsl:call-template name="classifierName">
              <xsl:with-param name="target" select="$target"/>
            </xsl:call-template>
          </xsl:attribute>
        </attribute>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:template name="operations">
    <xsl:variable name="operations" select="Foundation.Core.Classifier.feature/Foundation.Core.Operation"/>
    <xsl:if test="count($operations) &gt; 0">
      <xsl:for-each select="$operations">
        <operation>
          <xsl:attribute name="name">
            <xsl:value-of select="Foundation.Core.ModelElement.name"/>
          </xsl:attribute>
          <xsl:attribute name="type">
            <xsl:apply-templates select="Foundation.Core.BehavioralFeature.parameter/Foundation.Core.Parameter[Foundation.Core.Parameter.kind/@xmi.value='return']"/>
          </xsl:attribute>
          <xsl:attribute name="id">
            <xsl:value-of select="@xmi.id"/>
          </xsl:attribute>
          <xsl:apply-templates select="Foundation.Core.BehavioralFeature.parameter/Foundation.Core.Parameter[Foundation.Core.Parameter.kind/@xmi.value!='return']"/>
        </operation>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:template match="Foundation.Core.Parameter">
    <xsl:variable name="target" select="Foundation.Core.Parameter.type/*/@xmi.idref"/>
    <xsl:choose>
      <xsl:when test="Foundation.Core.Parameter.kind/@xmi.value='return'">
        <xsl:call-template name="classifierName">
          <xsl:with-param name="target" select="$target"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <parameter>
          <xsl:attribute name="name">
            <xsl:value-of select="Foundation.Core.ModelElement.name"/>
          </xsl:attribute>
          <xsl:attribute name="type">
            <xsl:call-template name="classifierName">
              <xsl:with-param name="target" select="$target"/>
            </xsl:call-template>
          </xsl:attribute>
        </parameter>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="signals">
    <xsl:variable name="signals" select="Foundation.Core.Classifier.feature/Behavioral_Elements.Common_Behavior.Reception"/>
    <xsl:if test="count($signals) &gt; 0">
      <xsl:for-each select="$signals">
        <signal>
          <xsl:attribute name="name">
            <xsl:value-of select="Foundation.Core.ModelElement.name"/>
          </xsl:attribute>
          <xsl:attribute name="id">
            <xsl:value-of select="Behavioral_Elements.Common_Behavior.Reception.signal/Behavioral_Elements.Common_Behavior.Signal/@xmi.idref"/>
          </xsl:attribute>
          <xsl:apply-templates select="Foundation.Core.BehavioralFeature.parameter/Foundation.Core.Parameter[Foundation.Core.Parameter.kind/@xmi.value!='return']"/>
        </signal>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>
  <xsl:template match="/">
    <suml version="0.2">
      <head>
        <meta name="exporter" value="{$exportername}" />
        <meta name="version" value="{$exporterversion}" />
      </head>
      <package name="{$modelname}">
        <xsl:apply-templates select="//Foundation.Core.DataType[@xmi.id]"/>
        <xsl:apply-templates select="//Foundation.Core.Class[@xmi.id]"/>
        <xsl:apply-templates select="//Foundation.Core.Association[@xmi.id]"/>
      </package>
    </suml>
  </xsl:template>
</xsl:stylesheet>
