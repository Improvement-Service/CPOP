
  var config = {
    apiKey: '4046f32b1e29040a42de6279781f4c78e7aef922',
    product: 'community',
    text : {
        thirdPartyTitle : 'Warning: Some cookies require your attention',
        thirdPartyDescription : 'Consent for some third party cookies can not be automatically revoked. Please follow the link below if you want to opt out of them.'
    },
    optionalCookies: [
      {
        name: 'analytics',
        label: 'Google Analytics',
        description: 'Analytical cookies help us to improve our website by collecting and reporting information on its usage.',
        cookies: ['_ga', '_gid', '_gat', '__utma', '__utmt', '__utmb', '__utmc', '__utmz', '__utmv'],
        onAccept: function(){
          gtag('consent', 'update', {'analytics_storage': 'granted'});
        },
        onRevoke: function(){
          gtag('consent', 'update', {'analytics_storage': 'denied'});
        }
      }
    ],
    
    position: 'RIGHT',
    theme: 'DARK'
  };
  
CookieControl.load( config );
  