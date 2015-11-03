function loginByName(){
	var name = document.getElementById("name").value.trim();
	var password = document.getElementById("password").value.trim();
	if(name.length==0||password.length==0){
		document.getElementById("nameMsg").style.display="block";
		document.getElementById("nameMsg").innerHTML="账号密码不能为空";
		return false;
	}
	var check = /^([0-9A-Za-z\-_\.]+)@([0-9a-z]+\.[a-z]{2,3}(\.[a-z]{2})?)$/g;
	var flag = name.match(check);
	var checkPhone = /^(1[3578])\d{9}$/;
	var flagPhone = name.match(checkPhone);
	if(flag==null&&flagPhone==null){
		document.getElementById("nameMsg").style.display="block";
		document.getElementById("nameMsg").innerHTML="账号不符合规则";
		return false;
	}
	//document.getElementById("loginNameForm").submit();
	$.ajax({  
	    type:"POST",
	    url:"/loginByName",
	    data:"name="+name+"&passwd="+password,
	    success:function(data){  
		   	if(data["if"]){
		   		window.location.href="/redirect?url="+document.getElementById("returnUrl").value
		   	}else{
		   		document.getElementById("nameError").style.display="block";
		   		document.getElementById("nameError").innerHTML = data["back"];
		   	}
		 }
	}); 
}

function loginByTel(){
	var phoneNum =  document.getElementById("phoneNum").value;
	var code =  document.getElementById("code").value;
	var check = /^(1[3578])\d{9}$/;
	var flag = phoneNum.match(check);
	if(phoneNum.length!=11||flag==null){
		document.getElementById("codeMsg").style.display="block";
		document.getElementById("codeMsg").innerHTML="电话号码不符合规则";
		return false;
	}
	if(code.length!=6){
		document.getElementById("codeMsg").style.display="block";
		document.getElementById("codeMsg").innerHTML="验证码为6位数字";
		return false;
	}

	$.ajax({
                         type:"POST",
                         url:"/loginByTel",
                         data:$("#loginTelForm").serialize(),
                         success:function(data){
                        	 if(data['if']){
                        		 window.location.href="/redirect?url="+document.getElementById("returnUrl").value
                        	 }else{
                        	 	document.getElementById("codeErrorMsg").style.display="block";
                        	 	document.getElementById("codeErrorMsg").innerHTML=data['back'];
                        	 }
                		 }
                  	});
}

function sendCode(){
	document.getElementById("codeMsg").style.display="none";
	var phoneNum =  document.getElementById("phoneNum").value;
	var imageCode =  document.getElementById("imageCode").value;
	if(imageCode==null||imageCode==""){
		document.getElementById("codeMsg").style.display="block";
		document.getElementById("codeMsg").innerHTML="图片验证码不能为空";
		return false;
	}
	var check = /^(13[0-9]|15[0-9]|18[0-9])\d{8}$/;
	var flag = phoneNum.match(check);
	if(phoneNum.length!=11||flag==null){
		document.getElementById("codeMsg").style.display="block";
		document.getElementById("codeMsg").innerHTML="电话号码不符合规则";
		return false;
	}
	document.getElementById("codeButton").disabled=true;
	$.ajax({  
         type:"POST",
         url:"/loginSendCode",
         data:"phoneNum="+phoneNum+"&imageCode="+imageCode,
         success:function(data){  
        	if(data['if']){
        		second = 180;
        		timer = setTimeout('change()',1000);
        	}else{
        		document.getElementById("codeButton").disabled=false;
        		document.getElementById("codeMsg").style.display="block";
        		document.getElementById("codeMsg").innerHTML=data['back'];
        	}
		 }
  	}); 
	getImageCode();
}

var second;
var timer;
function change()
{
  second--;
 
 if(second>-1)
 {
  	document.getElementById("codeButton").innerHTML="验证码已发送("+second+")";
  	document.getElementById("codeButton").disabled=true;
  	timer = setTimeout('change()',1000);//调用自身实现
 }
 else
 {
	 document.getElementById("codeButton").disabled=false;
     document.getElementById("codeButton").innerHTML="获取短信验证码";
     clearTimeout(timer);
 }
}

$(document).ready(function(){
	var map = document.getElementById("mapValue");
	if(map!=null){
		var mapValue = document.getElementById("mapValue").value;
		if(mapValue!=null){
			document.getElementById("phone_login").className="tab-pane fade";
			document.getElementById("login").className="tab-pane fade in active tab-fade-two";
			document.getElementById("phoneLi").className="l1";
			document.getElementById("nameLi").className="active l2";
		}
	}
});


document.onkeydown = function(e){
    if((e.keyCode || e.which) == 13){
    	var name = document.getElementById("name").value.trim();
    	if(name.length==0){
    		loginByTel();
    	}else{
    		loginByName();
    	}
    }
}

function onName(){
	document.getElementById("nameError").style.display="none";
	document.getElementById("nameMsg").style.display="none";
}

function onPhone(){
	document.getElementById("codeMsg").style.display="none";
	document.getElementById("codeErrorMsg").style.display="none";
}


function toPhoneLogin(){
	document.getElementById("name").value="";
	document.getElementById("password").value="";
}

function getImageCode() {
	document.getElementById("imageCodeImage").src="/getImageCodes/"+Math.round(Math.random()*1000000);
}
