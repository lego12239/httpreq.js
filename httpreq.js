/* httpreq | httpreq 0.10.0 | License - GNU LGPL 3 */
/*
  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

  https://github.com/lego12239/httpreq.js
*/
{
"use strict";

function httpreq(p_)
{
	var rpacks = [], i, j, reqs, p;
	
	if (!Array.isArray(p_))
		p = [p_];
	else
		p = p_;
	for(i = 0; i < p.length; i++) {
		if (!Array.isArray(p[i]))
			reqs = [p[i]];
		else
			reqs = p[i];
		rpacks[i] = {reqs: [], sem: 0};
		if ((reqs.length == 1) &&
		    (reqs[0].uri == null) &&
		    (reqs[0].cb != null))
			rpacks[i].reqs[0] = reqs[0];
		else
			for(j = 0; j < reqs.length; j++) {
				rpacks[i].reqs[j] = new httpreq.o(reqs[j]);
				rpacks[i].reqs[j].r.addEventListener("load",
				  httpreq.req_done.bind(this, rpacks, i, j));
			}
		rpacks[i].sem = reqs.length;
	}
	httpreq.rpack_do(rpacks, 0);
}

httpreq.p = {
	method: "GET",
	uri: "",
	is_async: true,
	user: "",
	password: "",
	headers: {},
	enctype: "application/x-www-form-urlencoded",
	data: {},
	resptype: "text",
	timeout: 0,
	ok_rex: /2\d\d/,
	cb: {
		_onok: undefined,
		_onnotok: undefined,
		_onfail: undefined,
		u_onabort: undefined,
		u_onerror: undefined,
		u_ontimeout: undefined,
		u_onload: undefined,
		u_onloadstart: undefined,
		u_onprogress: undefined,
		u_onloadend: undefined,
		onabort: undefined,
		onerror: undefined,
		ontimeout: undefined,
		onload: undefined,
		onloadstart: undefined,
		onprogress: undefined,
		onloadend: undefined},
	debug: 0
};

httpreq.err_msg = {
	"u_abort": "Upload aborted",
	"u_error": "Upload error: %s %s",
	"u_timeout": "Upload timeout reached",
	"abort": "Download aborted",
	"error": "Download error: %s %s",
	"status_err": "Got bad status %s %s %s",
	"readyState_err": "%s",
	"timeout": "Download timeout reached",
	"enctype_err": "Unknown encoding type %s"
};

httpreq.req_done = function (rpacks, i, j)
{
	//console.log("req " + i + "." + j + " is done");
	rpacks[i].sem--;
	if ((rpacks[i].sem == 0) && ((i + 1) < rpacks.length))
		httpreq.rpack_do(rpacks, i + 1);
}

httpreq.rpack_do = function (rpacks, i)
{
	var j;
	
	//console.log("do rpack " + i);
	if ((rpacks[i].reqs.length == 1) &&
	    !(rpacks[i].reqs[0] instanceof httpreq.o)) {
		rpacks[i].reqs[0].cb();
		httpreq.req_done(rpacks, i, 0);
	} else {
		for(j = 0; j < rpacks[i].reqs.length; j++)
			rpacks[i].reqs[j].go();
	}
}

httpreq.o = function (p)
{
	this.p = {};
	this.f = {json: 0};
	this.progress = {u: 0,
					 u_total: 0,
					 d: 0,
					 d_total: 0};


	this._set_prms(p);
	this._set_headers(p.headers);

	this.r = new XMLHttpRequest();
	this.r.upload.addEventListener("abort", this.u_onabort.bind(this));
	this.r.upload.addEventListener("error", this.u_onerror.bind(this));
	this.r.upload.addEventListener("load", this.u_onload.bind(this));
	this.r.upload.addEventListener("loadstart", this.u_onloadstart.bind(this));
	this.r.upload.addEventListener("progress", this.u_onprogress.bind(this));
	this.r.upload.addEventListener("timeout", this.u_ontimeout.bind(this));
	this.r.upload.addEventListener("loadend", this.u_onloadend.bind(this));

	this.r.addEventListener("abort", this.onabort.bind(this));
	this.r.addEventListener("error", this.onerror.bind(this));
	this.r.addEventListener("load", this.onload.bind(this));
	this.r.addEventListener("loadstart", this.onloadstart.bind(this));
	this.r.addEventListener("progress", this.onprogress.bind(this));
	this.r.addEventListener("timeout", this.ontimeout.bind(this));
	this.r.addEventListener("loadend", this.onloadend.bind(this));
}

httpreq.o.prototype._set_prms = function (p_in)
{
	var p, ret;


	this.__set_prms(p_in, this.p, httpreq.p);

	/* json must be processed with a help of JSON.parse(), because IE11- has no
	   support of responseType='json' */
	if ( this.p.resptype == "json" ) {
		this.p.resptype = "text";
		this.f.json = 1;
	}

	this.p.data = p_in.data;
	
	this._check_misspelled(p_in, this.p, "");
}

httpreq.o.prototype.__set_prms = function (p_in, p_out, p_def)
{
	var p, ret;


	if ( p_in == undefined )
		p_in = {};

	for(p in p_def)
		if ( Object.prototype.toString.call(p_def[p]) == "[object Object]" ) {
			p_out[p] = {};
			this.__set_prms(p_in[p], p_out[p], p_def[p]);
		} else
			this._set_prm(p, p_in[p], p_out, p_def);
}

httpreq.o.prototype._set_prm = function (n, v, p_out, p_def)
{
	if ( v == undefined )
		v = p_def[n];

	p_out[n] = v;
}

httpreq.o.prototype._check_misspelled = function (p_in, p_ex, parent)
{
	var p, ret;


	if ( p_in == undefined )
		p_in = {};

	for(p in p_in) {
		if (!p_ex.hasOwnProperty(p))
			throw("httpreq: unknown parameter name '" + parent + p + "'");
		if (Object.prototype.toString.call(p_in[p]) == "[object Object]" )
			this._check_misspelled(p_in[p], p_ex[p], parent + p + ".");
	}
}

httpreq.o.prototype._set_headers = function (headers)
{
	var n;


	for(n in headers)
		this.p.headers[n] = headers[n];
}

httpreq.o.prototype.u_onabort = function (ev)
{
	this._dbg_out("u_onabort()", ev);


	if ( this.p.cb.u_onabort != undefined )
		return this.p.cb.u_onabort(ev);
}

httpreq.o.prototype.u_onerror = function (ev)
{
	this._dbg_out("u_onerror(): " + this.r.status + ": " + this.r.statusText,
		ev);

	if ( this.p.cb.u_onerror != undefined )
		return this.p.cb.u_onerror(ev);
}

httpreq.o.prototype.u_onload = function (ev)
{
	this._dbg_out("u_onload()", ev);

	if ( this.p.cb.u_onload != undefined )
		return this.p.cb.u_onload(ev);

	/* WHAT IS MUST BE HERE? */

}

httpreq.o.prototype.u_onloadstart = function (ev)
{
	this._dbg_out("u_onloadstart()", ev);

	if ( this.p.cb.u_onloadstart != undefined )
		return this.p.cb.u_onloadstart(ev);
}

httpreq.o.prototype.u_onprogress = function (ev)
{
	if ( ev.lengthComputable ) {
		this.progress.u = ev.loaded;
		this.progress.u_total = ev.total;
	}

	this._dbg_out("u_onprogress()", ev);

	if ( this.p.cb.u_onprogress != undefined )
		return this.p.cb.u_onprogress(ev);
}

httpreq.o.prototype.u_ontimeout = function (ev)
{
	this._dbg_out("u_ontimeout()", ev);

	if ( this.p.cb.u_ontimeout != undefined )
		return this.p.cb.u_ontimeout(ev);
}

httpreq.o.prototype.u_onloadend = function (ev)
{
	this._dbg_out("u_onloadend()", ev);

	if ( this.p.cb.u_onloadend != undefined )
		return this.p.cb.u_onloadend(ev);
}

httpreq.o.prototype.onabort = function (ev)
{
	this._dbg_out("onabort()", ev);


	if ( this.p.cb.onabort != undefined )
		return this.p.cb.onabort(ev);

	this.onfail("abort");
}

httpreq.o.prototype.onerror = function (ev)
{
	this._dbg_out("onerror(): " + this.r.status + ": " + this.r.statusText,
		ev);

	if ( this.p.cb.onerror != undefined )
		return this.p.cb.onerror(ev);

	this.onfail("error", [this.r.status, this.r.statusText]);
}

httpreq.o.prototype.onload = function (ev)
{
	var data;


	this._dbg_out("onload()", ev);

	if ( this.p.cb.onload != undefined )
		return this.p.cb.onload(ev);

	if ( this.r.readyState == 4 ) {
		if ( this.p.ok_rex.test(this.r.status) ) {
			return this.onok();
		} else
			return this.onnotok();
	} else
		return this.onfail("readyState_err", ["readyState is not 4!"]);
}

httpreq.o.prototype.onloadstart = function (ev)
{
	this._dbg_out("onloadstart(): ", ev);

	if ( this.p.cb.onloadstart != undefined )
		return this.p.cb.onloadstart(ev);
}

httpreq.o.prototype.onprogress = function (ev)
{
	if ( ev.lengthComputable ) {
		this.progress.d = ev.loaded;
		this.progress.d_total = ev.total;
	}

	this._dbg_out("onprogress()", ev);

	if ( this.p.cb.onprogress != undefined )
		return this.p.cb.onprogress(ev);
}

httpreq.o.prototype.ontimeout = function (ev)
{
	this._dbg_out("ontimeout()", ev);

	if ( this.p.cb.ontimeout != undefined )
		return this.p.cb.ontimeout(ev);

	this.onfail("timeout");
}

httpreq.o.prototype.onloadend = function (ev)
{
	this._dbg_out("onloadend()", ev);

	if ( this.p.cb.onloadend != undefined )
		return this.p.cb.onloadend(ev);
}

httpreq.o.prototype.onok = function ()
{
	var data;


	if ( this.p.cb._onok == undefined )
		return;

	data = this.f.json ? JSON.parse(this.r.responseText) : this.r.response;

	return this.p.cb._onok(data);
}

httpreq.o.prototype.onnotok = function ()
{
	/* Use Error object because may be we will want to throw an exception
	   in onfail callback and handle all exceptions globally in a common way.
	   So in this case we need everywhere an Error object for errors */
	var err = new Error();
	var data;


	err.code = this.r.status;
	err.name = this.r.statusText;
	/* May be an error is returned by a server as a json? */
	if ( this.f.json ) {
		try {
			err.message = JSON.parse(this.r.responseText);
		} catch (e) {
			err.message = this.r.responseText;
		}
	} else
		err.message = this.r.responseText;
	err.toString = httpreq.statuserror_toString;

	console.error("httpreq: " + err);

	if ( this.p.cb._onnotok != undefined )
		this.p.cb._onnotok(err);
}

httpreq.o.prototype.onfail = function (err_name, err_msg_args)
{
	/* Use Error object because may be we will want to throw an exception
	   in onfail callback and handle all exceptions globally in a common way.
	   So in this case we need everywhere an Error object for errors */
	var err = new Error();


	err.name = "httpreq." + err_name;
	if ( httpreq.err_msg[err_name] == undefined )
		throw("httpreq: error message is not defined for the error: " +
			err_name);
	err.message = httpreq.err_msg[err_name];
	err.msg_args = err_msg_args;
	err.toString = httpreq.error_toString;

	console.error("httpreq: " + err);

	if ( this.p.cb._onfail != undefined )
		this.p.cb._onfail(err);
}

httpreq.o.prototype.go = function (data)
{
	var reqprms;


	if ( data == undefined )
		data = this.p.data;

	/* May be data is HTMLFormElement. If so, get from it
	   method, uri and enctype. */
	this._set_req_prms(data);

	if (( this.p.method.toUpperCase() == "GET" ) ||
		( this.p.method.toUpperCase() == "HEAD" ) ||
		( this.p.method.toUpperCase() == "DELETE" ))
		this._send_data_in_uri(data);
	else
		this._send_data_as_payload(data);
}

httpreq.o.prototype._set_req_prms = function (data)
{
	var otype;


	otype = Object.prototype.toString.call(data);

	if ( otype == "[object HTMLFormElement]") {
		this.p.method = data.method;
		this.p.uri = data.action;
		this.p.enctype = data.enctype;
	}
}

httpreq.o.prototype._send_data_in_uri = function (data)
{
	var data_to_send, uri = this.p.uri;


	data_to_send = this._fmt_data(data, "application/x-www-form-urlencoded");
	if ( data_to_send != "" )
		if ( uri.indexOf("?") == -1 )
			uri += "?" + data_to_send;
		else
			if ( uri.charAt(uri.length - 1) == "&" )
				uri += data_to_send;
			else
				uri += "&" + data_to_send;

	this.r.open(this.p.method, uri, this.p.is_async,
		this.p.user, this.p.password);
	this.r.responseType = this.p.resptype;
	this.r.timeout = this.p.timeout;
	this._set_r_headers();
	this.r.send(null);
	//this.r.abort();
	
}

httpreq.o.prototype._send_data_as_payload = function (data)
{
	var data_to_send;


	data_to_send = this._fmt_data(data, this.p.enctype);

	this.r.open(this.p.method, this.p.uri, this.p.is_async,
		this.p.user, this.p.password);
	this.r.responseType = this.p.resptype;
	this.r.timeout = this.p.timeout;
	this._set_r_headers();
	this.r.send(data_to_send);
	//this.r.abort();
	
}

/*
 * Format a data to send accordingly to enctype.
 * The function has a side effect: set this.p.headers["Content-Type"].
 */
httpreq.o.prototype._fmt_data = function (data, enctype)
{
	var otype;


	if ( data == undefined )
		return "";

	otype = Object.prototype.toString.call(data);

	switch (otype) {
		case "[object HTMLFormElement]":
			return this._fmt_data_form(data, this._get_fmt_funs(enctype));
		case "[object Object]":
			return this._fmt_data_obj(data, this._get_fmt_funs(enctype));
		default:
			/* If data is Blob, FormData, ArrayBufferView and etc */
			return data;
	}
}

httpreq.o.prototype._get_fmt_funs = function (enctype)
{
	if ( this.fmt_funs[enctype] == undefined )
		return this.onfail("enctype_err", [enctype]);
	else
		return this.fmt_funs[enctype];
}

httpreq.o.prototype._fmt_data_form = function (data, funs)
{
	var i, prms = [];


	for(i = 0; i < data.length; i++)
		prms.push(funs.p.call(this, data[i].name, data[i].value));

	return funs.p_join.call(this, prms);
}

httpreq.o.prototype._fmt_data_obj = function (data, funs)
{
	var i, prms = [];


	for(i in data)
		if ( Array.isArray(data[i]) ) {
			for(j = 0; j < data[i].length; j++)
				if (data[i][j] != null)
					prms.push(funs.p.call(this, i, data[i][j]));
		} else
			if (data[i] != null)
				prms.push(funs.p.call(this, i, data[i]));

	return funs.p_join.call(this, prms);
}

httpreq.o.prototype.__fmt_prm_urlencoded = function (name, value)
{
	return encodeURIComponent(name) + "=" + encodeURIComponent(value);
}

httpreq.o.prototype.__fmt_prms_urlencoded = function (prms)
{
	this.p.headers["Content-Type"] = "application/x-www-form-urlencoded";
	return prms.join("&");
}

httpreq.o.prototype.__fmt_prm_plain = function (name, value)
{
	return name.replace(/[=\\]/g, "\\$&").replace(/\n/g, "\\n") + "=" +
		value.replace(/[=\\]/g, "\\$&").replace(/\n/g, "\\n");
}

httpreq.o.prototype.__fmt_prms_plain = function (prms)
{
	this.p.headers["Content-Type"] = "text/plain";
	return prms.join("\r\n");
}

httpreq.o.prototype.__fmt_prm_multipart = function (name, value)
{
	return "Content-Disposition: form-data; name=\"" +
		name + "\"\r\n\r\n" + value + "\r\n";
}

httpreq.o.prototype.__fmt_prms_multipart = function (prms)
{
	var boundary, res = "";


	boundary = this.__mk_multipart_boundary(prms);
	res = "--" + boundary;
	res += prms.join("--" + boundary + "\r\n");
	res += "--" + boundary + "--\r\n";

	this.p.headers["Content-Type"] = "multipart/form-data; boundary=" +
		boundary;
	return res;
}

httpreq.o.prototype.__mk_multipart_boundary = function (prms)
{
	// HERE MUST BE SOMETHING MORE RELIABLE
	return "---------------------------" + Date.now().toString(16);
}

httpreq.o.prototype._set_r_headers = function ()
{
	var n;


	for(n in this.p.headers)
		this.r.setRequestHeader(n, this.p.headers[n]);
}

httpreq.o.prototype._dbg_out = function (msg, ev)
{
	var str = "httpreq:";


	if ( ! this.p.debug )
		return;

	//console.log(ev);

	str += " U[" + this.progress.u + "/" + this.progress.u_total + "]";
	str += "D[" + this.progress.d + "/" + this.progress.d_total + "]";

	str += " readyState=" + this.r.readyState;
	str += " " + msg;

	console.log(str);
}


/**********************************************************************
 * A custom toString() for Error object
 **********************************************************************/
httpreq.error_toString = function ()
{
	var msg;
	var msg_args = this.msg_args;
	var i = -1;
	
	
	msg = this.message.replace(/%s/g, function (m) { return msg_args[++i]; });

	return this.name + ": " + msg;
}

httpreq.statuserror_toString = function ()
{
	return this.code + " " + this.name + ": " + this.message;
}

}

/**********************************************************************
 * Parameters
 **********************************************************************/
httpreq.o.prototype.fmt_funs = {
	"application/x-www-form-urlencoded": {
		p: httpreq.o.prototype.__fmt_prm_urlencoded,
		p_join: httpreq.o.prototype.__fmt_prms_urlencoded },
	"multipart/form-data": {
		p: httpreq.o.prototype.__fmt_prm_multipart,
		p_join: httpreq.o.prototype.__fmt_prms_multipart },
	"text/plain": {
		p: httpreq.o.prototype.__fmt_prm_plain,
		p_join: httpreq.o.prototype.__fmt_prms_plain }
};

