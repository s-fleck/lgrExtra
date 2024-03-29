% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AppenderDigest.R
\name{AppenderMail}
\alias{AppenderMail}
\title{Abstract class for email Appenders}
\value{
Abstract classes cannot be instantiated with \verb{$new()} and therefore
do not return anything. They are solely for developers that want to write
their own extension to \strong{lgr}.
}
\description{
\strong{Abstract classes}, only exported for package developers.
}
\seealso{
Other abstract classes: 
\code{\link{AppenderDigest}}

Other Digest Appenders: 
\code{\link{AppenderDigest}},
\code{\link{AppenderPushbullet}},
\code{\link{AppenderSendmail}}
}
\concept{Digest Appenders}
\concept{abstract classes}
\section{Super classes}{
\code{\link[lgr:Filterable]{lgr::Filterable}} -> \code{\link[lgr:Appender]{lgr::Appender}} -> \code{\link[lgr:AppenderMemory]{lgr::AppenderMemory}} -> \code{\link[lgrExtra:AppenderDigest]{lgrExtra::AppenderDigest}} -> \code{AppenderMail}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{to}}{\code{character} vector. The email addresses of the recipient}

\item{\code{from}}{\code{character} vector. The email address of the sender}

\item{\code{cc}}{\code{character} vector. The email addresses of the cc-recipients (carbon copy)}

\item{\code{bcc}}{\code{character} vector. The email addresses of bcc-recipients (blind carbon copy)}

\item{\code{html}}{\code{logical} scalar.  Send a html email message?
This does currently only format the log contents as monospace verbatim
text.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AppenderMail-new}{\code{AppenderMail$new()}}
\item \href{#method-AppenderMail-set_to}{\code{AppenderMail$set_to()}}
\item \href{#method-AppenderMail-set_from}{\code{AppenderMail$set_from()}}
\item \href{#method-AppenderMail-set_cc}{\code{AppenderMail$set_cc()}}
\item \href{#method-AppenderMail-set_bcc}{\code{AppenderMail$set_bcc()}}
\item \href{#method-AppenderMail-set_html}{\code{AppenderMail$set_html()}}
\item \href{#method-AppenderMail-format}{\code{AppenderMail$format()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="lgr" data-topic="Filterable" data-id="add_filter"><a href='../../lgr/html/Filterable.html#method-Filterable-add_filter'><code>lgr::Filterable$add_filter()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="Filterable" data-id="filter"><a href='../../lgr/html/Filterable.html#method-Filterable-filter'><code>lgr::Filterable$filter()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="Filterable" data-id="remove_filter"><a href='../../lgr/html/Filterable.html#method-Filterable-remove_filter'><code>lgr::Filterable$remove_filter()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="Filterable" data-id="set_filters"><a href='../../lgr/html/Filterable.html#method-Filterable-set_filters'><code>lgr::Filterable$set_filters()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="Appender" data-id="set_layout"><a href='../../lgr/html/Appender.html#method-Appender-set_layout'><code>lgr::Appender$set_layout()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="Appender" data-id="set_threshold"><a href='../../lgr/html/Appender.html#method-Appender-set_threshold'><code>lgr::Appender$set_threshold()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="append"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-append'><code>lgr::AppenderMemory$append()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="clear"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-clear'><code>lgr::AppenderMemory$clear()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="flush"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-flush'><code>lgr::AppenderMemory$flush()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="set_buffer_size"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-set_buffer_size'><code>lgr::AppenderMemory$set_buffer_size()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="set_flush_on_exit"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-set_flush_on_exit'><code>lgr::AppenderMemory$set_flush_on_exit()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="set_flush_on_rotate"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-set_flush_on_rotate'><code>lgr::AppenderMemory$set_flush_on_rotate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="set_flush_threshold"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-set_flush_threshold'><code>lgr::AppenderMemory$set_flush_threshold()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="set_should_flush"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-set_should_flush'><code>lgr::AppenderMemory$set_should_flush()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgr" data-topic="AppenderMemory" data-id="show"><a href='../../lgr/html/AppenderMemory.html#method-AppenderMemory-show'><code>lgr::AppenderMemory$show()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lgrExtra" data-topic="AppenderDigest" data-id="set_subject_layout"><a href='../../lgrExtra/html/AppenderDigest.html#method-AppenderDigest-set_subject_layout'><code>lgrExtra::AppenderDigest$set_subject_layout()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AppenderMail-new"></a>}}
\if{latex}{\out{\hypertarget{method-AppenderMail-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AppenderMail$new(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AppenderMail-set_to"></a>}}
\if{latex}{\out{\hypertarget{method-AppenderMail-set_to}{}}}
\subsection{Method \code{set_to()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AppenderMail$set_to(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AppenderMail-set_from"></a>}}
\if{latex}{\out{\hypertarget{method-AppenderMail-set_from}{}}}
\subsection{Method \code{set_from()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AppenderMail$set_from(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AppenderMail-set_cc"></a>}}
\if{latex}{\out{\hypertarget{method-AppenderMail-set_cc}{}}}
\subsection{Method \code{set_cc()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AppenderMail$set_cc(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AppenderMail-set_bcc"></a>}}
\if{latex}{\out{\hypertarget{method-AppenderMail-set_bcc}{}}}
\subsection{Method \code{set_bcc()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AppenderMail$set_bcc(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AppenderMail-set_html"></a>}}
\if{latex}{\out{\hypertarget{method-AppenderMail-set_html}{}}}
\subsection{Method \code{set_html()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AppenderMail$set_html(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AppenderMail-format"></a>}}
\if{latex}{\out{\hypertarget{method-AppenderMail-format}{}}}
\subsection{Method \code{format()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AppenderMail$format(color = FALSE, ...)}\if{html}{\out{</div>}}
}

}
}
